open Soup;

open Cohttp_lwt_unix;

open Lwt.Infix;

module ScrapingResult = {
  type t =
    | Finish
    | Error(Cohttp_lwt_unix.Response.t);
};

module Error = {
  type criticalError =
    | UnhandledAutosuggestionLevel
    | UnhandledAutosuggestionResponse
    | AutoSuggestQueryError;
  type t =
    | CriticalError(criticalError)
    | BadAutosuggestResponse
    | NoAutosuggestResults;
};

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

module Filters = {
  module Keys = {
    let category = "mieszkania";
    let typ = "wynajem";
    let price = "filter_float_price";
    let size = "filter_float_m";
    let enumRoomsNum = (~index) =>
      Printf.sprintf("[filter_enum_rooms_num][%d]", index);
    let locations = (~index, ~key) =>
      Printf.sprintf("locations[%d][%s]", index, key);
  };
  module Rooms = {
    type t =
      | One
      | Two
      | Three
      | Four
      | Five
      | Six
      | Seven
      | Eight
      | NineAndMore;
    module Set =
      Set.Make({
        type nonrec t = t;
        let compare = compare;
      });
    let to_int =
      fun
      | One => 1
      | Two => 2
      | Three => 3
      | Four => 4
      | Five => 5
      | Six => 6
      | Seven => 7
      | Eight => 8
      | NineAndMore => 9;
    let toQueryParams = rooms => {
      Set.to_seq(rooms)
      |> List.of_seq
      |> List.mapi((index, room) =>
           (Keys.enumRoomsNum(~index), to_int(room) |> string_of_int)
         );
    };
  };
  module Locations = {
    type locationLevel = string;
    type id = string;
    type t = {
      index: int,
      data: list((locationLevel, id)),
    };
    let fromQueryString = (~index, ~searchTerm) => {
      let autosuggestURL =
        Uri.of_string(
          {|https://www.otodom.pl/ajax/geo6/autosuggest?data=%s|},
        );
      Client.get(
        Uri.add_query_param(autosuggestURL, ("data", [searchTerm])),
      )
      >>= (
        ((response, body)) =>
          switch (Response.status(response)) {
          | `OK =>
            Cohttp_lwt.Body.to_string(body)
            >|= (
              jsonString => {
                Error.(
                  switch (Yojson.Safe.from_string(jsonString)) {
                  | `List([`Assoc(dict), ..._]) =>
                    let locationKeys =
                      switch (List.assoc_opt("level", dict)) {
                      | Some(`String("REGION")) => Ok(["region_id"])
                      | Some(`String("SUBREGION")) =>
                        Ok(["region_id", "subregion_id"])
                      | Some(`String("CITY")) =>
                        Ok(["region_id", "subregion_id", "city_id"])
                      | Some(`String("DISTRICT")) =>
                        Ok([
                          "region_id",
                          "subregion_id",
                          "city_id",
                          "district_id",
                        ])
                      | Some(`String("STREET")) =>
                        Ok([
                          "region_id",
                          "subregion_id",
                          "city_id",
                          "district_id",
                          "street_id",
                        ])
                      | Some(_) =>
                        Error(CriticalError(UnhandledAutosuggestionLevel))
                      | None =>
                        Error(CriticalError(UnhandledAutosuggestionResponse))
                      };
                    Base.Result.bind(locationKeys, ~f=locationFragments =>
                      try({
                        let data =
                          List.map(
                            key =>
                              switch (List.assoc_opt(key, dict)) {
                              | Some(`String(id)) => (key, id)
                              | _ => raise_notrace(Not_found)
                              },
                            locationFragments,
                          );
                        Ok({index, data});
                      }) {
                      | Not_found =>
                        Error(CriticalError(UnhandledAutosuggestionResponse))
                      }
                    );
                  | `List([]) => Error(NoAutosuggestResults)
                  | _ =>
                    Error(CriticalError(UnhandledAutosuggestionResponse))
                  }
                );
              }
            )
          | _ => Lwt.return(Error(Error.BadAutosuggestResponse))
          }
      );
    };
    let toQueryParams = (~index, x) =>
      List.map(((key, id)) => (Keys.locations(~index, ~key), id), x);
  };
  module Range = {
    type t = {
      from: option(float),
      to_: option(float),
    };
    let toQueryParams = (~key, {from, to_}) => {
      let map = (~suffix, value) => {
        switch (value) {
        | Some(x) => [(key ++ suffix, x |> string_of_float)]
        | None => []
        };
      };
      List.append(map(~suffix=":from", from), map(~suffix=":to", to_));
    };
  };
};

module Query = {
  let make = (~locations, ~priceRange, ~sizeRange, ~noOfRooms, ~page) => {
    open Filters;
    let baseURL = Uri.make(~scheme="https", ~host="www.otodom.pl", ());
    let params =
      [
        Locations.toQueryParams(~index=0, locations),
        Range.toQueryParams(~key=Keys.price, priceRange),
        Range.toQueryParams(~key=Keys.size, sizeRange),
        Rooms.toQueryParams(noOfRooms),
        [("nrAdsPerPage", "72"), ("page", string_of_int(page))],
      ]
      |> List.flatten;
    Uri.add_query_params'(baseURL, params);
  };
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
      | `Moved_permanently => Lwt.return(ScrapingResult.Finish)
      | _ => Lwt.return(ScrapingResult.Error(response))
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
