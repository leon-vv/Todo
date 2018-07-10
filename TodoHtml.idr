module TodoHtml

import Todo
import Html
import Record
import File

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

idToString : Todo -> String
idToString t = case (t .. "id") of
                    Nothing => ""
                    Just id => show id

-- Try generating form based on schema
todoForm : Todo -> Html
todoForm t =
  let checked = if t .. "done" then [("checked", "")] else []
  in tagac "form"
      [("method", "get"), ("action", "./save")]
      [
        input Hidden [("name", "id"), ("value", idToString t)],
        input Checkbox (("name", "done")::checked),
        input TextType [("name", "name"), ("value", t .. "name")],
        input Submit [("value", "Save")]]

divId : String -> List Html -> Html 
divId id children = tagac "div" [("id", id)] children

spanId : String -> List Html -> Html 
spanId id children = tagac "span" [("id", id)] children

link : String -> String -> Html
link href content = tagac "a" [("href", href)] [text content]

withinContent : List Html -> JS_IO String
withinContent b = 
  (\style => "<!DOCTYPE html>" ++ show (
                tagc "html" [
                    tagc "head" [tagac "style" [("type", "text/CSS")] [text style]],
                    tagc "body" [divId "wrapper" [divId "content" b]]]))
    <$> readFileSync "style.css"

todoToHtml : Todo -> Html
todoToHtml t =
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in tagac "div" [("class", "todo")]
    [ tagac "form" [("action", ("/delete?id=" ++ idToString t))]
        [tagac "button" [("type", "submit")] [text "Delete"]],
      link ("/edit?id=" ++ idToString t) "Edit",
      tagc "span" [text name],
      tagc "div" [taga {selfClose=True} "input" ([("type", "checkbox")] ++ checked)]]

messageToHtml : String -> Html
messageToHtml msg = tagc "div" [text msg]

showTodos : List Todo -> JS_IO String
showTodos ts =
  let add = divId "add" [tagac "a" [("href", "/new")] [text "Add Todo"]]
  in let header = divId "header" [spanId "name" [text "Description"], spanId "done" [text "Done?"]]
  in let todos = divId "todos" (header::(map todoToHtml ts))
  in withinContent [add, todos]



