module MultiSelect.MultiSelect exposing (view)

import Css exposing (focus, hover, property)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import MultiSelect.Config exposing (..)
import MultiSelect.State exposing (..)
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Util exposing (..)

import Tailwind.Color as Tw
import Tailwind.Theme as Tw
import Tailwind.Utilities as Tw

view : Config a msg -> State a -> Html msg
view (Config c) (State s) =
    div [ css [ Tw.w_full, Tw.flex, Tw.flex_col, Tw.items_center, Tw.mx_auto ]
        , class c.class
        ]
        [ div [ css [ Tw.w_full, Tw.mx_auto ] ]
            [ div [ css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.relative  ] ]
                [ div [ css [ Tw.w_full ] ]
                    [ div [ css [Tw.flex, Tw.border, Tw.border_color Tw.gray_200, Tw.bg_color Tw.white, Tw.rounded ] ]
                        [ viewSelectedItems (Config c) (State s)
                        , viewInputSearch (Config c) (State s)
                        , viewButtonShow (Config c) (State s)
                        ]
                    ]
                , div
                    [ css (iff s.isOpened [ Tw.absolute, Tw.shadow, Tw.top_full, Tw.bg_color Tw.white, Tw.z_40, Tw.w_full, Tw.left_0, Tw.rounded, Tw.max_h_32, Tw.overflow_y_auto, Tw.mt_2 ] [ Tw.hidden ])
                    ]
                    [ viewItems (Config c) (State s)
                    ]
                ]
            ]
        ]


viewInputSearch : Config a msg -> State a -> Html msg
viewInputSearch (Config c) (State s) =
    div [ css [ Tw.mx_4, Tw.grow ] ]
        [ input
            [ type_ "text"
            , css [ Tw.form_input, Tw.w_full, Tw.border_0, Tw.rounded_md, focus [ Tw.border_color Tw.blue_400, Tw.ring, Tw.ring_color (Tw.withOpacity Tw.opacity50 Tw.blue_200), Css.property "appearance" "none" ] ]
            , placeholder c.placeholder
            , value s.search
            , Html.Styled.Attributes.fromUnstyled (onInput (\x -> c.pipe <| State { s | search = x }))
            , Html.Styled.Attributes.fromUnstyled (onFocus (c.pipe (State { s | isOpened = not s.isOpened })))
            , onFocusIn (c.pipe (State { s | isOpened = True }))
            ]
            []
        ]


onFocusIn : msg -> Attribute msg
onFocusIn msg = Html.Styled.Attributes.fromUnstyled <|
    on "focusin" (Decode.succeed msg)


viewButtonShow : Config a msg -> State a -> Html msg
viewButtonShow (Config c) (State s) =
    div [ css [ Tw.text_color Tw.gray_300, Tw.w_8, Tw.py_1, Tw.pl_2, Tw.pr_1, Tw.border_l, Tw.flex, Tw.items_center, Tw.border_color Tw.gray_200 ] ]
        [ button
            [ css [ Tw.cursor_pointer, Tw.w_6, Tw.h_6, Tw.text_color Tw.gray_600, Tw.outline_none, focus [ Tw.outline_none ] ]
            , id "select_dropdown"
            , Html.Styled.Attributes.fromUnstyled <| onClick (c.pipe (State { s | isOpened = not s.isOpened }))
            ]
            [ iff s.isOpened iconDown iconUp
            ]
        ]


viewSelectedItems : Config a msg -> State a -> Html msg
viewSelectedItems (Config c) (State s) =
    div [ css [ Tw.shrink, Tw.flex, Tw.flex_wrap ] ] <|
        List.map
            (viewSelectedItem c.getValue
                (\x -> c.pipe <| State { s | selected = removeItem c s x })
            )
            s.selected


viewSelectedItem : (a -> String) -> (a -> msg) -> a -> Html msg
viewSelectedItem getValue click x =
    div
        [ css [ Tw.flex, Tw.justify_center, Tw.items_center, Tw.m_1, Tw.font_medium, Tw.py_1, Tw.px_2, Tw.bg_color Tw.white, Tw.rounded_full, Tw.text_color Tw.blue_700, Tw.bg_color Tw.blue_100, Tw.border, Tw.border_color Tw.blue_300 ]
        ]
        [ div [ css [ Tw.text_xs, Tw.font_normal, Tw.leading_none, Tw.max_w_full, Tw.flex_initial ] ]
            [ text (getValue x) ]
        , div
            [ css [ Tw.text_xs, Tw.font_normal, Tw.leading_none, Tw.max_w_full, Tw.flex_initial ]
            , Html.Styled.Attributes.fromUnstyled <| onClick (click x)
            ]
            [ iconDelete ]
        ]


viewItems : Config a msg -> State a -> Html msg
viewItems (Config c) (State s) =
    div [ css [ Tw.flex, Tw.flex_col, Tw.w_full ] ] <|
        List.map
            (\x ->
                let
                    isSelected =
                        List.member x s.selected

                    click =
                        \z ->
                            c.pipe
                                (State
                                    { s
                                        | selected =
                                            if isSelected then
                                                removeItem c s z

                                            else
                                                List.append s.selected [ z ]
                                    }
                                )
                in
                viewItem c.getValue click x isSelected
            )
            (filter c s)


viewItem : (a -> String) -> (a -> msg) -> a -> Bool -> Html msg
viewItem getValue click x enable =
    div
        [ css [ Tw.cursor_pointer, Tw.w_full, Tw.border_color Tw.gray_100, Tw.rounded_t, Tw.border_b, hover [ Tw.bg_color Tw.blue_100 ] ]
        , id "select_dropdown"
        , Html.Styled.Attributes.fromUnstyled <| onClick (click x)
        ]
        [ div
            [ css (iff enable [ Tw.flex, Tw.w_full, Tw.items_center, Tw.p_2, Tw.pl_2, Tw.border_l_2, Tw.relative, Tw.border_color Tw.blue_600 ] [ Tw.flex, Tw.w_full, Tw.items_center, Tw.p_2, Tw.pl_2, Tw.border_l_2, Tw.relative, hover [ Tw.border_color Tw.blue_100 ] ])
            ]
            [ div [ css [ Tw.w_full, Tw.items_center, Tw.flex ] ]
                [ div [ css [ Tw.mx_2, Tw.leading_6 ] ]
                    [ text (getValue x) ]
                ]
            ]
        ]


removeItem c s x =
    List.filter (\y -> c.getID y /= c.getID x) s.selected


filter c s =
    s.data
        |> List.filter
            (\x ->
                String.contains (String.toLower s.search)
                    (String.toLower (c.getValue x))
            )


iconUp : S.Svg msg
iconUp =
    S.svg
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "none"
        , SA.viewBox "0 0 24 24"
        , SA.stroke "currentColor"
        , SA.strokeWidth "2"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.css [ Tw.w_4, Tw.h_4 ]
        ]
        [ S.polygon [ SA.points "18 15 12 9 6 15" ] [] ]


iconDown : S.Svg msg
iconDown =
    S.svg
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "none"
        , SA.viewBox "0 0 24 24"
        , SA.stroke "currentColor"
        , SA.strokeWidth "2"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        , SA.css [ Tw.w_4, Tw.h_4 ]
        ]
        [ S.polygon [ SA.points "6 9 18 9 12 15" ] [] ]


iconDelete : S.Svg msg
iconDelete =
    S.svg
        [ SA.css [ Tw.cursor_pointer, hover [ Tw.text_color Tw.blue_400 ], Tw.rounded_full, Tw.w_4, Tw.h_4, Tw.ml_2 ]
        , SA.width "100%"
        , SA.height "100%"
        , SA.fill "none"
        , SA.viewBox "0 0 24 24"
        , SA.stroke "currentColor"
        , SA.strokeWidth "2"
        , SA.strokeLinecap "round"
        , SA.strokeLinejoin "round"
        ]
        [ S.line [ SA.x1 "18", SA.y1 "6", SA.x2 "6", SA.y2 "18" ] []
        , S.line [ SA.x1 "6", SA.y1 "6", SA.x2 "18", SA.y2 "18" ] []
        ]
