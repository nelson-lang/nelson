%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function tf = is_interrupted_test(testCase)
  ctrlCDetected = any(contains(testCase.msg, _('Interrupt (ctrl-c) encountered.')));
  ctrlCDetected = any(ctrlCDetected(:));
  interruptedProcess = any(testCase.res_cmd(:) == interrupted_process_code());
  interruptedProcess = any(interruptedProcess(:));
  tf = interruptedProcess || ctrlCDetected;
end
%=============================================================================
