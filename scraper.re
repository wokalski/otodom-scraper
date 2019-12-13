open Soup;

open Cohttp_lwt_unix;

open Lwt.Infix;

type result =
  | Finish
  | Error(Cohttp_lwt_unix.Response.t);

type listing = {
  image: string,
  location: string,
  url: string,
  price: string,
  size: string,
};

let print_listing = x =>
  String.concat(
    ",",
    [
      "=IMAGE(\"" ++ x.image ++ "\")",
      x.url,
      x.price,
      "\"" ++ x.location ++ "\"",
      "\"" ++ x.size ++ "\"",
    ],
  )
  |> print_endline;

module Str = Re_str;

let imageRegex = Str.regexp("background-image:url(\\(.*\\))");

let locationRegex = Str.regexp(".* Wrocław, \\(.*\\)");

let numberRegex = Re.Pcre.re("\\d") |> Re.compile;

let urlRegex = Str.regexp("\\(#.*\\)");

let require = (desc, str) =>
  try(require(str)) {
  | exc =>
    Printf.fprintf(stderr, "%s", desc);
    raise(exc);
  };

let extractListings = x => {
  let listings = x $ "div[class=\"listing\"]";
  let articles = listings $$ "article" |> to_list;
  List.map(
    article => {
      let header = article $ "header[class*=\"offer-item-header\"]";
      let url =
        header
        $ "h3"
        $ "a"
        |> attribute("href")
        |> require("url")
        |> Str.replace_first(urlRegex, "");
      let location =
        header
        $ "p"
        |> trimmed_texts
        |> String.concat("")
        |> Str.replace_first(locationRegex, "\\1");
      let price =
        article
        $ "li[class*=\"offer-item-price\"]"
        |> leaf_text
        |> require("price")
        |> Re.all(numberRegex)
        |> List.map(x => x |> Re.Group.all |> Array.to_list)
        |> List.concat
        |> String.concat("");
      let imageSpan = article $ "span[class*=\"img-cover\"]";
      let imageDataSrc = imageSpan |> attribute("data-src");

      let imageStyle =
        imageSpan
        |> attribute("style")
        |> Base.Option.map(~f=Str.replace_first(imageRegex, "\\1"));

      let image =
        switch (imageStyle, imageDataSrc) {
        | (Some(img), None)
        | (None, Some(img)) => img
        | _ =>
          Printf.fprintf(
            stderr,
            "%s: %s",
            "image",
            Soup.pretty_print(article $ "span[class*=\"img-cover\"]"),
          );
          raise(Not_found);
        };
      let size =
        article
        $ "li[class*=\"hidden-xs offer-item-area\"]"
        |> leaf_text
        |> Base.Option.value(~default="nieznany_rozmiar");
      {url, location, price, image, size};
    },
    articles,
  );
};

let rec fetchSite = (baseURL, page) => {
  let fullURL =
    if (page <= 1) {
      baseURL;
    } else {
      baseURL ++ "&page=" ++ string_of_int(page);
    };
  Client.get(Uri.of_string(fullURL))
  >>= (
    ((response, body)) =>
      switch (Response.status(response)) {
      | `OK =>
        Cohttp_lwt.Body.to_string(body)
        >|= Soup.parse
        >|= extractListings
        >|= List.iter(print_listing)
        >>= (_ => fetchSite(baseURL, page + 1))
      | `Moved_permanently => Lwt.return(Finish)
      | _ => Lwt.return(Error(response))
      }
  );
};

let fetchSites = baseURL => fetchSite(baseURL, 1);

let rec parseBodies =
  fun
  | [] => Lwt.return([])
  | [h, ...t] =>
    parseBodies(t)
    >>= (
      links =>
        Cohttp_lwt.Body.to_string(h)
        >>= (x => Lwt.return([Soup.parse(x), ...links]))
    );

let extractData = url =>
  fetchSites(url)
  >>= (
    fun
    | Finish => Lwt.return([])
    | Error(_) => Lwt.return([])
  )
  >|= ignore;

Arg.parse(
  [
    ("--url", Arg.String(s => Lwt_main.run(extractData(s))), "otodom url"),
  ],
  ignore,
  "",
);
