module TodoHtml

import Todo
import Html
import Record
import File

import Effects

%access public export

notFound : Html
notFound = tagc "div" [text "404 not found"]

idToString : Todo -> String
idToString t = case (t .. "id") of
                    Nothing => ""
                    Just id => show id

-- Try generating form based on schema
todoForm : Todo -> Html
todoForm t =
  let checked = if t .. "done" then [("checked", "")] else []
  in tagac "form"
      [("id", "edit"), ("method", "get"), ("action", "./save")]
      [
        input Hidden [("name", "id"), ("value", idToString t)],
        tagc "div" [
          input TextType [("name", "name"), ("value", t .. "name"), ("placeholder", "description")]],
        tagc "label" [text "Already done:", input Checkbox (("name", "done")::checked)],
        tagc "div" [input Submit [("value", "Save")]]]

emptyForm : Html
emptyForm = todoForm ({ "id" ::= Nothing & "name" ::= "" & "done" ::= False })

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
                  tagc "head" [tagac "style" [("type", "text/CSS")] [text {escape=False} style]],
                  tagc "body" [divId "wrapper" [divId "content" b]]]))
    <$> readFileSync "style.css"

todoToHtml : Todo -> Html
todoToHtml t =
  let name = (t .. "name")
  in let checked = if t .. "done" then [("checked", "")] else []
  in tagac "div" [("class", "todo")]
    [ tagc "span" [text name],
      tagc "div" [taga {selfClose=True} "input" ([("disabled", ""), ("type", "checkbox")] ++ checked)],
      link ("/edit?id=" ++ idToString t) "Edit",
      tagac "form" [("action", ("/delete?id=" ++ idToString t)), ("method", "get")]
        [tagac "button" [("type", "submit")] [text "Delete"],
         taga {selfClose=True} "input"
          [("type", "hidden"), ("value", idToString t), ("name", "id")]]]

messageToHtml : String -> Html
messageToHtml msg = tagc "div" [text msg]

showTodos : List Todo -> JS_IO String
showTodos ts =
  let add = divId "add" [tagac "a" [("href", "/new")] [text "Add Todo"]]
  in let header = divId "header" [spanId "name" [text "Description"], spanId "done" [text "Done?"]]
  in let todos = divId "todos" (header::(map todoToHtml ts))
  in withinContent [add, todos]



