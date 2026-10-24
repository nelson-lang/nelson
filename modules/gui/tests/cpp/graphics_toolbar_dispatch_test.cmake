file(READ "${NELSON_TEST_REPOSITORY_DIR}/modules/graphics_qt/src/cpp/GOWindowActions.cpp"
  GOWINDOW_SOURCE
)

string(REGEX
  MATCH
  "GOWindow::onNewFigureAction\\(\\)[^{]*\\{[^}]*\\}"
  NEW_FIGURE_ACTION
  "${GOWINDOW_SOURCE}"
)

if(NOT NEW_FIGURE_ACTION MATCHES "postCommand\\(L\"figure\\(\\)\"\\)")
  message(FATAL_ERROR
    "The New Figure toolbar action must dispatch figure() to the interpreter."
  )
endif()

if(NEW_FIGURE_ACTION MATCHES "createNewFigure")
  message(FATAL_ERROR
    "The New Figure toolbar action must not mutate graphics state on the GUI thread."
  )
endif()
