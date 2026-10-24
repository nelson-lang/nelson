%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if ~ismodule('dynamic_link')
  return;
end
filename = fullfile(modulepath('nelson', 'builtin'), ['libnlsEngine', getdynlibext()]);
info = jsondecode(__library_dependencies__(filename));
imports = lower(string([info.needed; info.delayed]));
asserts.isfalse(any(contains(imports, 'libnlsweb_gui')));
asserts.isfalse(any(contains(imports, 'libnlswebview')));
%=============================================================================
