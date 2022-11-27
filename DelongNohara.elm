module Delong2Nohara exposing (..)

import Browser
import Html exposing (Html, a, div, h1, input, p, table, td, text, th, tr)
import Html.Attributes as HA
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { xC : String
    , xSi : String
    , xMn : String
    , xCr : String
    , xNi : String
    , xMo : String
    , xTi : String
    , xCu : String
    , xN : String
    }


init : Model
init =
    Model "0.045" "0.45" "1.25" "18.15" "8.55" "0.05" "0.02" "0.15" "0.045"



-- UPDATE


type Msg
    = MxC String
    | MxSi String
    | MxMn String
    | MxCr String
    | MxNi String
    | MxMo String
    | MxTi String
    | MxCu String
    | MxN String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MxC xC ->
            { model | xC = xC }

        MxSi xSi ->
            { model | xSi = xSi }

        MxMn xMn ->
            { model | xMn = xMn }

        MxCr xCr ->
            { model | xCr = xCr }

        MxNi xNi ->
            { model | xNi = xNi }

        MxMo xMo ->
            { model | xMo = xMo }

        MxTi xTi ->
            { model | xTi = xTi }

        MxCu xCu ->
            { model | xCu = xCu }

        MxN xN ->
            { model | xN = xN }


headset hname =
    th
        [ HA.style "font-size" "14px"
        , HA.style "color" "white"
        , HA.style "background-color" "DarkCyan"
        ]
        [ text hname ]


xnam mod =
    [ "%C", "%Si", "%Mn", "%Cr", "%Ni", "%Mo", "%Ti", "%Cu", "%N" ]


xmod mod =
    [ mod.xC, mod.xSi, mod.xMn, mod.xCr, mod.xNi, mod.xMo, mod.xTi, mod.xCu, mod.xN ]


xmsg mod =
    [ MxC, MxSi, MxMn, MxCr, MxNi, MxMo, MxTi, MxCu, MxN ]


trio mod =
    zip3 (xnam mod) (xmod mod) (xmsg mod)


essential nam x ms =
    let
        s =
            [ HA.style "padding-left" "10px"
            , HA.style "font-size" "16px"
            , HA.style "width" "60px"
            ]
    in
    td s [ viewInput "string" nam x ms ]


valset mod =
    List.map (\( nc, cc, mc ) -> essential nc cc mc) (trio mod)


view : Model -> Html Msg
view model =
    div [ HA.style "margin" "5em" ]
        [ h1 [] [ text "Calculator for Austenitic Stainless Steels" ]
        , table [ HA.style "border" "2px solid black" ]
            [ tr []
                (List.map headset
                    (xnam model)
                )
            , tr [] (valset model)
            ]
        , p [] [ text ("Md30 Temperature (Nohara) = " ++ usDec3 (tempMd30 model) ++ "°C") ]
        , p [] [ text ("Ferrite Number FN  = " ++ usDec3 (fnA model) ++ " (austenitic stainless)") ]
        , p [] [ text ("Pitting Corrosion Resistance (PRE) = " ++ usDec3 (eqPittingResistance model)) ]
        , p [ HA.style "margin-top" "10em" ] []
        , p [] []
        , text "This page was built with "
        , a [ HA.href "https://elm-lang.org/" ] [ text "Elm programming language." ]
        , p [] [ text "© 2022 J. Lammi" ]
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ HA.type_ t, HA.placeholder p, HA.value v, HA.style "width" "60px", onInput toMsg ] []


zip : List a -> List b -> List ( a, b )
zip a b =
    List.map2 (\x y -> ( x, y )) a b


zip3 : List a -> List b -> List c -> List ( a, b, c )
zip3 a b c =
    List.map3 (\x y z -> ( x, y, z )) a b c


tupleprod : ( number, number ) -> number
tupleprod ( c, x ) =
    c * x


sumOfProducts : List number -> List number -> number
sumOfProducts alist blist =
    zip alist blist
        |> List.map tupleprod
        |> List.sum



{-
   /* Nohara formula  */
   /*  tmd30 = 551 - 462 C - 9.2 Si - 8.1 Mn - 13.7 Cr - 29 Ni   */
   /*  - 18.5 Mo - 29 Cu - 68 Nb - 462 N - 1.42 (GS -8.0)        */
-}


xval s =
    Maybe.withDefault 0.0 (String.toFloat s)


tempMd30 : Model -> Float
tempMd30 mod =
    let
        coefficients =
            [ 462.0, 9.2, 8.1, 13.7, 29.0, 18.5, 29.0, 462.0 ]

        elements =
            [ mod.xC, mod.xSi, mod.xMn, mod.xCr, mod.xNi, mod.xMo, mod.xCu, mod.xN ]

        composition =
            List.map xval elements
    in
    551.0 - sumOfProducts coefficients composition



{-
   // Avesta FN without % Co
   crekv= 1.0*form.anCr.value + 1.5* form.anSi.value + 1.0*form.anMo.value + 2.0* form.anTi.value + 0.5* form.anNb.value;

   niekv= 1.0*form.anNi.value + 0.5*form.anMn.value + 30.0*form.anN.value + 30.0*form.anC.value + 0.5*form.anCu.value;
   // While FNA<= 5.9
   fn = 3.34*crekv - 2.46*niekv -28.6;
   // While FNA>= 6.0
   if (fn>5.99) {fn = 4.44*crekv - 3.39*niekv -38.4; fm='b'}
   // While FNA>= 12.0
   if (fn>11.99) {fn = 4.06*crekv - 3.23*niekv -32.2; fm='c'}
-}
-- DELTA FERRITE NUMBER FN


fnA : Model -> Float
fnA mod =
    let
        crEkv =
            xval mod.xCr
                + 1.5
                * xval mod.xSi
                + xval mod.xMo
                + 2.0
                * xval mod.xTi

        niEkv =
            xval mod.xNi
                + 0.5
                * xval mod.xMn
                + 30.0
                * xval mod.xC
                + 30.0
                * xval mod.xN
                + 0.5
                * xval mod.xCu

        preFNA =
            3.34 * crEkv - 2.46 * niEkv - 28.6
    in
    if preFNA < 6.0 then
        preFNA

    else if preFNA < 12.0 then
        4.44 * crEkv - 3.39 * niEkv - 38.4

    else
        4.06 * crEkv - 3.23 * niEkv - 32.2



-- PITTING RESISTANCE equivalence
-- // Pitting corrosion resistance:
-- r = mod.xCr.value + 3.3*mod.xMo.value + 20.0*mod.xN.value;


eqPittingResistance : Model -> Float
eqPittingResistance mod =
    let
        coefficients =
            [ 1.0, 3.3, 20.0 ]

        elements =
            [ mod.xCr, mod.xMo, mod.xN ]

        composition =
            List.map xval elements
    in
    sumOfProducts coefficients composition



-- DECIMAL NUMBERS FORMAT


sharesLocale =
    { usLocale
        | decimals = Exact 6
        , negativePrefix = "-"
        , positivePrefix = " "
    }


sharesLocaleUS =
    { sharesLocale
        | decimals = Exact 3
    }


sharesLocale3 =
    { sharesLocale
        | decimals = Exact 3
    }


cutDec6 x =
    format sharesLocale x


cutDec3 x =
    format sharesLocale3 x


usDec3 x =
    format sharesLocaleUS x
