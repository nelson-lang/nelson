file(READ "${NELSON_TEST_REPOSITORY_DIR}/modules/gui/src/cpp/DesktopDocking.cpp"
  DOCKING_SOURCE
)
string(FIND "${DOCKING_SOURCE}" "DesktopUndockWindow(QMainWindow*" UNDOCK_START)
string(FIND
  "${DOCKING_SOURCE}"
  "DesktopShowDockedWindow(QMainWindow*"
  UNDOCK_END
)
if(UNDOCK_START LESS 0 OR UNDOCK_END LESS UNDOCK_START)
  message(FATAL_ERROR "Cannot locate the desktop undocking implementation.")
endif()
math(EXPR UNDOCK_LENGTH "${UNDOCK_END} - ${UNDOCK_START}")
string(SUBSTRING
  "${DOCKING_SOURCE}"
  ${UNDOCK_START}
  ${UNDOCK_LENGTH}
  UNDOCK_SOURCE
)
set(PREVIOUS_POSITION -1)
foreach(STEP
  IN
  ITEMS
  "client->saveState()"
  "client->setParent(nullptr)"
  "client->restoreState(clientLayout)"
  "client->restoreGeometry(geometry)"
  "client->layout()->activate()"
)
  string(FIND "${UNDOCK_SOURCE}" "${STEP}" POSITION)
  if(POSITION LESS 0 OR POSITION LESS_EQUAL PREVIOUS_POSITION)
    message(FATAL_ERROR
      "Missing or misplaced desktop layout restoration: ${STEP}"
    )
  endif()
  set(PREVIOUS_POSITION ${POSITION})
endforeach()
