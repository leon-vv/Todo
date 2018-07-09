module TodoHtml

import Todo
import Html
import Record

%access public export

notFound : Html
notFound = tagc "div" [text "404 not found"]

emptyForm : Html
emptyForm =
  tagac "form"
      [("method", "get"), ("action", "./save")]
      [ input Checkbox [("name", "done")],
        input TextType [("name", "name")],
        input Submit [("value", "Save")]]

-- Try generating form based on schema
todoForm : Todo -> Html
todoForm t =
  let checked = if t .. "done" then [("checked", "")] else []
  in tagac "form"
      [("method", "get"), ("action", "./save")]
      [
        input Hidden [("name", "id"), ("value", show $ t .. "id")],
        input Checkbox (("name", "done")::checked),
        input TextType [("name", "name"), ("value", t .. "name")],
        input Submit [("value", "Save")]]


withinBody : List Html -> String
withinBody b = "<!DOCTYPE html>" ++ show (
  tagc "html" [
    tagc "body" b])

todoToHtml : Todo -> Html
todoToHtml t =
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in tagc "div" [
        text name, 
        taga {selfClose=True} "input" ([("type", "checkbox")] ++ checked)]

messageToHtml : String -> Html
messageToHtml msg = tagc "div" [text msg]

showTodos : String -> List Todo -> String
showTodos msg ts =
  let body = map todoToHtml ts
  in if msg == "" then withinBody body
                  else withinBody (messageToHtml msg::body)



