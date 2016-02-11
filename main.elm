import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, targetValue)
import Graphics.Element exposing (..)
import Graphics.Input as GI exposing (..)
import Graphics.Input.Field as GIF exposing (..)
import Json.Decode exposing ((:=))
import Task
import Http
import Text
import Color
import String
import Window

main : Signal Element
main = Signal.map5 view Window.dimensions username.signal pw.signal data.signal answer.signal

username : Signal.Mailbox Content
username = Signal.mailbox noContent

pw : Signal.Mailbox Content
pw = Signal.mailbox noContent     
      
data : Signal.Mailbox String
data = Signal.mailbox ""

answer : Signal.Mailbox String
answer = Signal.mailbox ""
         
message : Json.Decode.Decoder String
message = "message" := Json.Decode.string

queryRegister : Signal.Mailbox (String, String)
queryRegister = Signal.mailbox ("", "")
                
port reg: Signal.Signal (Task.Task x ())
port reg = Signal.map (uncurry register) queryRegister.signal
  
register : String -> String -> Task.Task x ()
register user pass =
  if String.length user == 0 || String.length pass == 0 then
    Signal.send answer.address ""
  else
    (Http.post message "http://forkback.herokuapp.com/register" (Http.string ("{\"username\":\"" ++ user ++ "\",\"password\":\"" ++ pass ++ "\"}")) |> Task.toResult)
      `Task.andThen` (\task -> case task of
                                 Err e -> Signal.send answer.address (toString e)
                                 Ok s -> Signal.send answer.address s)

querySubmit : Signal.Mailbox (String, String, String)
querySubmit = Signal.mailbox ("", "", "")

port sub : Signal.Signal (Task.Task x ())
port sub = Signal.map (\(username, password, data) -> submit username password data) querySubmit.signal              

submit : String -> String -> String -> Task.Task x ()
submit user pass data =
  if String.length user + String.length pass + String.length data == 0 then
    Signal.send answer.address ""
  else
    (Http.post message "http://forkback.herokuapp.com/submit" (Http.string ("{\"username\":\"" ++ user ++ "\",\"password\":\"" ++ pass ++ "\", \"data\":\"" ++ data ++ "\"}")) |> Task.toResult)
      `Task.andThen` (\task -> case task of
                                 Err e -> Signal.send answer.address (toString e)
                                 Ok s -> Signal.send answer.address s)      

queryScores : Signal.Mailbox ()
queryScores = Signal.mailbox ()

port sco : Signal.Signal (Task.Task x ())
port sco = Signal.map scores queryScores.signal

scores : () -> Task.Task x ()
scores () =
  (Http.get message "http://forkback.herokuapp.com/" |> Task.toResult)
      `Task.andThen` (\task -> case task of
                                 Err e -> Signal.send answer.address (toString e)
                                 Ok s -> Signal.send answer.address s)
         
view : (Int, Int) -> Content -> Content -> String -> String -> Element       
view (w, h) user pass input answer =
  flow right
         [ flow down
             [ container w 20 middle (Text.fromString "HELLO" |> centered)
             , flow right
                 [ field defaultStyle (Signal.message username.address) "Username" user
                 , password defaultStyle (Signal.message pw.address) "Password" pass
                 ] |> container w 40 middle
             , flow right
                 [ GI.button (Signal.message queryRegister.address (user.string, pass.string)) "Register"
                 , spacer 20 40
                 , GI.button (Signal.message queryScores.address ()) "Scores"
                 , spacer 20 40
                 , GI.button (Signal.message querySubmit.address (user.string, pass.string, input)) "Submit"
                 ] |> container w 40 middle
             , textarea [myStyle, value input, on "input" targetValue (Signal.message data.address)] [] |> toElement (w//2) h |> container w h midTop
             ] |> container (5 * w // 6) h midTop
         , container (w//6) h midTop (Text.fromString answer |> centered)
         ]
             
myStyle : Attribute
myStyle = 
  style
    [ ("height", "60%")
    , ("width", "100%")
    ]
