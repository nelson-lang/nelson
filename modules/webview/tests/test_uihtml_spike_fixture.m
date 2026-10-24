%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% The uihtml spike page loads a relative stylesheet and a relative script.
% An installed Nelson must ship every one of them, otherwise setup() never runs
% and test_uihtml_spike fails with no useful diagnostic.
pageDir = [modulepath('webview', 'tests'), '/uihtml_spike_page'];
asserts.istrue(isfile([pageDir, '/page.html']));
asserts.istrue(isfile([pageDir, '/page.css']));
asserts.istrue(isfile([pageDir, '/page.js']));
%=============================================================================
% Guard the installer audit on the source tree: module.iss is not shipped.
moduleIss = [modulepath('webview'), '/module.iss'];
if isfile(moduleIss)
  installer = fileread(moduleIss);
  asserts.istrue(contains(installer, 'tests\*.js'));
end
%=============================================================================
