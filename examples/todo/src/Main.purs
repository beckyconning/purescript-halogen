module Main where

import Data.Void
import Data.Tuple
import Data.Maybe
import Data.Either
import Data.Array (zipWith, length, modifyAt, deleteAt, (..), (!!))

import qualified Data.String as S

import Debug.Trace

import Control.Functor (($>))
import Control.Alternative
import Control.Bind
import Control.Monad.Eff

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Routing as Routing
import qualified Routing.Hash as Routing

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.Events.Handler as E

import qualified Halogen.Themes.Bootstrap3 as B
import qualified Halogen.Themes.Bootstrap3.InputGroup as BI

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

postRender :: forall eff. Input -> HTMLElement -> Driver Input eff -> Eff (HalogenEffects eff) Unit
postRender (ChangeRoute s) _ _ = Routing.setHash s
postRender _ _ _               = pure unit

type Todo = { description :: String, completed :: Boolean }

-- | The state of the application
data AppState = Splash | TodoList [Todo]

-- | Inputs to the state machine
data Input
  = ChangeRoute String
  | NewTodo String
  | UpdateDescription Number String
  | MarkCompleted Number Boolean
  | RemoveTodo Number

-- | The view is a state machine, consuming inputs, and generating HTML documents which in turn, generate new inputs
ui :: forall m. (Alternative m) => Component m Input Input
ui = render <$> stateful Splash update
  where
  initialState :: AppState
  initialState = TodoList []

  render :: AppState -> H.HTML (m Input)
  render appState =
    H.div [ A.class_ B.container ]
          [ router appState ]

  router :: AppState -> H.HTML (m Input)
  router Splash           = renderSplash
  router (TodoList todos) = renderTodoList todos

  renderSplash :: H.HTML (m Input)
  renderSplash =
    H.div [ A.class_ B.jumbotron ]
          [ H.h1     []
                     [ H.text "PureScript Todo" ]

          , H.p      []
                     [ H.text "100% To Do, 0% MVC" ]

          , H.button [ A.classes [ B.btn, B.btnPrimary ]
                     , A.onClick (A.input_ (ChangeRoute "todo-list"))
                     ]
                     [ H.text "Continue" ]
          ]

  renderTodoList :: [Todo] -> H.HTML (m Input)
  renderTodoList todos =
    H.div []
          [ H.h1 [ A.id_ "header" ] [ H.text "Todo list" ]
          , renderToolbar
          , H.div_ (zipWith renderTodo todos (0 .. length todos))
          ]

  renderToolbar :: H.HTML (m Input)
  renderToolbar = H.p [ A.class_ B.btnGroup ]
                   [ H.button [ A.classes [ B.btn, B.btnPrimary ]
                              , A.onClick (A.input_ $ NewTodo "")
                              ]
                              [ H.text "New Todo" ]
                   ]

  renderTodo :: Todo -> Number -> H.HTML (m Input)
  renderTodo todo index =
    H.p []
        [ H.div [ A.class_ B.inputGroup ]
                [ H.span [ A.class_ B.inputGroupAddon ]
                         [ H.input  [ A.class_ B.checkbox
                                    , A.type_ "checkbox"
                                    , A.checked todo.completed
                                    , A.title "Mark as completed"
                                    , A.onChecked (A.input (MarkCompleted index))
                                    ]
                                    []
                         ]

               , H.input  [ A.classes [ B.formControl ]
                          , A.placeholder "Description"
                          , A.onValueChanged (A.input (UpdateDescription index))
                          , A.value todo.description
                          ]
                          []

               , H.span [ A.class_ B.inputGroupBtn ]
                        [ H.button [ A.classes [ B.btn, B.btnDefault ]
                                   , A.title "Remove task"
                                   , A.onClick (A.input_ $ RemoveTodo index)
                                   ]
                                   [ H.text "✖" ]
                        ]
               ]
        ]

  update :: AppState -> Input -> AppState
  update _ (ChangeRoute "todo-list") = TodoList []

  update _ (ChangeRoute _) = Splash

  update (TodoList todos) (NewTodo description) = TodoList (todos ++ [newTodo])
    where
    newTodo = { description: description, completed: false }

  update (TodoList todos) (UpdateDescription i description) = TodoList $ modifyAt i updateDesc todos
    where
    updateDesc :: Todo -> Todo
    updateDesc todo = todo { description = description }

  update (TodoList todos) (MarkCompleted i completed) = TodoList $ modifyAt i updateCompleted todos
    where
    updateCompleted :: Todo -> Todo
    updateCompleted todo = todo { completed = completed }

  update (TodoList todos) (RemoveTodo i) = TodoList $ deleteAt i 1 todos

main = do
  Tuple node driver <- runUIWith ui postRender
  appendToBody node
  Routing.hashChanged (\oldHash newHash -> driver (ChangeRoute newHash))
