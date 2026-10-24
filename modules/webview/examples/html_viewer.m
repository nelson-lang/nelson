%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Open a small self-contained page in a new HTML viewer.
page = ['text://<!doctype html><html><body style="font-family:system-ui;padding:2rem">', ...
  '<h1>Nelson HTML viewer</h1><p>This page was created by a script.</p></body></html>'];
web(page, '-new');
