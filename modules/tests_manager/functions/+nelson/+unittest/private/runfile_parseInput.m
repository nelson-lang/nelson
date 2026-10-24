%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function filename = runfile_parseInput(args)
  filename = args{1};
  if length(args) == 2
    logical(args{2});
  end
  if ~isfile(filename)
    error('Nelson:tests_manager:fileDoesNotExist', 'An existing filename expected.');
  end
end
%=============================================================================
