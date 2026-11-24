%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WINDOWS ONLY-->
%=============================================================================
% WebView2Loader.dll must be deployed next to the Nelson binaries: WebUI
% resolves it at run time with LoadLibraryA("WebView2Loader.dll"), so having the
% WebView2 Runtime installed system-wide is NOT enough. When the file is missing
% the embedded WebView silently degrades to an app-mode browser window, which
% looks almost identical but runs out of process, so system dialogs cannot be
% owned by the editor and the window outlives Nelson. Guard the deployment here
% because that failure is invisible at run time.
%=============================================================================
bin_dir = modulepath('nelson', 'bin');
loader = [bin_dir, '/WebView2Loader.dll'];
assert_istrue(isfile(loader), [_('Missing WebView2 loader: '), loader]);
%=============================================================================
% A truncated or placeholder file would load but fail to create the environment.
info = dir(loader);
assert_istrue(info.bytes > 10000);
%=============================================================================
