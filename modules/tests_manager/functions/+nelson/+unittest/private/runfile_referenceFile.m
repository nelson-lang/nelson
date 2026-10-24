%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function dia_ref = runfile_referenceFile(filename, test_run_options)
  dia_ref = '';
  if ~test_run_options.check_ref
    return
  end
  [p, f, ~] = fileparts(filename); %#ok<NLS0004>
  dia_ref = [p, '/', f, '.ref'];
  if ~isfile(dia_ref)
    error('Nelson:tests_manager:referenceDiaryExpected', 'reference diary expected.');
  end
end
%=============================================================================
