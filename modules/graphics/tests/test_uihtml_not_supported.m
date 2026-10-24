%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% uihtml hosts a live HTML page, which only the WebView desktop can display.
% Everywhere else it must say so plainly instead of handing back a handle that
% would never show anything.
%=============================================================================
skip_testsuite(strcmp(getnelsonmode(), 'WEB_GUI'), _('this test covers the modes without a WebView desktop.'));
%=============================================================================
asserts.isfalse(isempty(which('uihtml')));
asserts.isfalse(isempty(which('sendEventToHTMLSource')));
%=============================================================================
asserts.checkerror('uihtml();', getString(message('Nelson:uihtml:webviewDesktopRequired')), ...
  'Nelson:uihtml:webviewDesktopRequired');
%=============================================================================
% A parent argument must not change the verdict.
f = uifigure('Visible', 'off');
c = onCleanup(@() close(f));
asserts.checkerror('uihtml(f);', getString(message('Nelson:uihtml:webviewDesktopRequired')), ...
  'Nelson:uihtml:webviewDesktopRequired');
%=============================================================================
% sendEventToHTMLSource rejects anything that is not a uihtml object.
asserts.checkerror('sendEventToHTMLSource(f, ''ping'');', getString(message('Nelson:sendEventToHTMLSource:invalidHandle')), ...
  'Nelson:sendEventToHTMLSource:invalidHandle');
%=============================================================================
