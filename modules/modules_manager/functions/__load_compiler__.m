%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __load_compiler__()
  if ismodule('compiler'); return; end
  directory = fullfile(nelsonroot(), 'modules', 'compiler');
  if ~isfolder(directory)
    error('Nelson:nelsonc:moduleMissing', 'The optional compiler build module is not installed.');
  end
  try
    addmodule(directory, 'compiler');
  catch ex
    if ismodule('compiler'); removemodule('compiler'); end
    rethrow(ex);
  end
end
