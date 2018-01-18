module TodoHtml

import Todo
import Html
import Record

%access public export

notFound : Html
notFound = tagc "div" [text "404 not found"]

-- Try generating form based on schema
todoForm : Html
todoForm =
  tagac "form"
      [("method", "post"), ("action", "./")]
      [
        taga {selfClose=True} "input" [("type", "text"), ("name", "name")],
        taga {selfClose=True} "input" [("type", "submit")]]


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
  in show $ if msg == "" then withinBody body
                         else withinBody (messageToHtml msg::body)



