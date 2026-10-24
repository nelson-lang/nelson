%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Argument rejection must not load the optional module or graphical services.
loaded = ismodule('compiler');
for name = {'deploytool', 'standaloneApplicationCompiler'}
  asserts.istrue(endsWith(which(name{1}), [name{1}, '.m']));
  failed = false;
  try; feval(name{1}, '-build'); catch ex; failed = ~isempty(ex.identifier); end
  asserts.istrue(failed);
  failed = false;
  try; output = feval(name{1}); catch ex; failed = ~isempty(ex.identifier); end
  asserts.istrue(failed);
  asserts.isequal(ismodule('compiler'), loaded);
end
