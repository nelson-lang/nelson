%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function system = __nelsonc_is_system_library__(name, filename)
  name = lower(name);
  system = ~isempty(regexp(name, '^(api|ext)-[a-z0-9-]+-l[0-9]+-[0-9]+-[0-9]+\.dll$', 'once'));
  if system || isempty(filename)
    return;
  end
  if ~isempty(regexp(name, '^(msvcp|msvcr|vcruntime|concrt|vcomp|vccorlib|vcamp)[0-9].*\.dll$', 'once'))
    system = true;
    return;
  end
  root = getenv('SystemRoot');
  if ~isempty(root)
    root = lower(strrep(fullpath(root), '\', '/'));
    filename = lower(strrep(fullpath(filename), '\', '/'));
    system = startsWith(filename, [root, '/']);
  end
end
