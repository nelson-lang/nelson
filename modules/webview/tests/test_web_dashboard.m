%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% <--INTERACTIVE TEST-->
%=============================================================================
skip_testsuite(exist('web') == 0, 'Web dashboard demo requires the webview.');
%=============================================================================
webDashboardSampleCount = 80;
run([modulepath('webview', 'root'), '/examples/web_dashboard.m']);

% The Nelson series feeds the generated page.
assert_isequal(numel(webDashboardSeries), webDashboardSampleCount);
% The self-contained HTML page was written to disk before opening.
assert_istrue(isfile(webDashboardFile));
pageText = fileread(webDashboardFile);
assert_istrue(contains(pageText, 'canvas'));
%=============================================================================
