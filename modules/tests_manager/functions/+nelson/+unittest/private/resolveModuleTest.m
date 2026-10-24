%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function test_file = resolveModuleTest(tests_dir, test_name)
  test_file = '';
  if ~ischar(test_name) || isempty(test_name)
    return
  end
  [p, f, e] = fileparts(test_name); %#ok<NLS0004>
  if ~isempty(p)
    return
  end
  if isempty(e)
    test_file = [tests_dir, f, '.m'];
  elseif strcmp(e, '.m')
    test_file = [tests_dir, f, e];
  end
end
