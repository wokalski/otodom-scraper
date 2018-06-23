open Soup;

open Cohttp;

open Cohttp_lwt_unix;

open Lwt.Infix;

type propertyType =
  | Office
  | Flat;

let propertyType =
  try (
    switch Sys.argv.(1) {
    | "office" => Office
    | _ => Flat
    }
  ) {
  | _ => Flat
  };

let baseURL =
  switch propertyType {
  | Office => Sys.getenv("OFFICE_SEARCH_URL")
  | Flat => Sys.getenv("FLAT_SEARCH_URL")
  };

type result =
  | Finish (list Cohttp_lwt_body.t)
  | Error Cohttp_lwt_unix.Response.t;

let rec fetchSite baseURL page body_list => {
  let fullURL =
    if (page <= 1) {
      baseURL
    } else {
      baseURL ^ "&page=" ^ string_of_int page
    };
  Client.get (Uri.of_string fullURL)
  >>= (
    fun (response, body) =>
      switch (Response.status response) {
      | `OK => fetchSite baseURL (page + 1) [body, ...body_list]
      | `Moved_permanently => Lwt.return (Finish body_list)
      | _ => Lwt.return (Error response)
      }
  )
};

let fetchSites baseURL => fetchSite baseURL 1 [];

let rec parseBodies =
  fun
  | [] => Lwt.return []
  | [h, ...t] =>
    parseBodies t
    >>= (
      fun links =>
        Cohttp_lwt_body.to_string h >>= (fun x => Lwt.return [Soup.parse x, ...links])
    );

type listing = {
  image: string,
  location: string,
  url: string,
  price: string,
  size: string
};

module Str = Re_str;

let imageRegex = Str.regexp "background-image:url(\\(.*\\))";

let locationRegex = Str.regexp ".* Warszawa, \\(.*\\)";

let numberRegex = Re_pcre.re "\\d" |> Re.compile;

let urlRegex = Str.regexp "\\(#.*\\)";

let extractListings l =>
  List.map
    (
      fun x => {
        let listings = x $ "div[class=\"listing\"]";
        let articles = listings $$ "article" |> to_list;
        List.map
          (
            fun article => {
              let header = article $ "header[class=\"offer-item-header\"]";
              let url =
                header
                $ "h3"
                $ "a"
                |> attribute "href"
                |> require
                |> Str.replace_first urlRegex "";
              let location =
                header
                $ "p"
                |> leaf_text
                |> require
                |> Str.replace_first locationRegex "\\1";
              let price =
                article
                $ "li[class=\"offer-item-price\"]"
                |> leaf_text
                |> require
                |> Re.all numberRegex
                |> List.map (fun x => x |> Re.get_all |> Array.to_list)
                |> List.concat
                |> String.concat "";
              let image =
                article
                $ "span[class=\"img-cover\"]"
                |> attribute "style"
                |> require
                |> Str.replace_first imageRegex "\\1";
              let size =
                article
                $ "li[class=\"hidden-xs offer-item-area\"]"
                |> leaf_text
                |> require;
              {url, location, price, image, size}
            }
          )
          articles
      }
    )
    l
  |> List.concat;

let extractData =
  fetchSites baseURL
  >>= (
    fun
    | Finish bodies => parseBodies bodies
    | Error _ => Lwt.return []
  )
  >>= (fun x => Lwt.return (extractListings x));

let print_listing x =>
  String.concat
    "|"
    [
      x.image,
      x.url,
      x.price,
      x.location,
      ...switch propertyType {
         | Office => [x.size]
         | Flat => []
         }
    ]
  |> print_endline;

Lwt_main.run extractData |> List.iter print_listing;
