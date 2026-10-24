%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function snapshot = mexception_behavior_snapshot()
  isNelson = exist('nelsonroot', 'builtin') == 5;
  if isNelson
    addpath([nelsonroot(), '/modules/error_manager/tests']);
    addpath([nelsonroot(), '/modules/error_manager/functions']);
    rehash();
  end

  ME = MException('Nelson:test:format', 'Hello %s %d', 'world', 7);
  cause = MException('Nelson:test:cause', 'Cause message.');
  withCause = ME.addCause(cause);
  if isNelson
    correction = fun_MException_correction();
    withCorrection = addCorrection(ME, correction);
  else
    withCorrection = ME;
  end

  snapshot.class = class(ME);
  snapshot.isobject = isobject(ME);
  snapshot.properties = properties(ME);
  snapshot.methods = methods(ME);
  snapshot.fieldnames = fieldnames(ME);
  snapshot.identifier = ME.identifier;
  snapshot.message = ME.message;
  snapshot.causeSize = size(ME.cause);
  snapshot.stackSize = size(ME.stack);
  snapshot.stackClass = class(ME.stack);
  snapshot.correctionSize = size(ME.Correction);
  snapshot.correctionClass = class(withCorrection.Correction);
  snapshot.addCauseSize = size(withCause.cause);
  snapshot.addCauseClass = class(withCause.cause{1});
  snapshot.eqSelf = ME == ME;
  snapshot.eqCause = ME == withCause;
  snapshot.isequalSelf = isequal(ME, ME);
  snapshot.isequalCause = isequal(ME, withCause);
  snapshot.reportBasic = getReport(ME, 'basic');

  try
    error('Nelson:test:last', 'Last message.')
  catch caught
    snapshot.caughtClass = class(caught);
  end

  try
    edit MException
  catch editException
    snapshot.editError = editException.message;
  end
end
%=============================================================================
