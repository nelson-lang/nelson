%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
savedPath = path();
addpath([modulepath('interpreter', 'tests'), '/classdef']);
restorePath = onCleanup(@() path(savedPath));
savedWarnings = warning();
restoreWarnings = onCleanup(@() warning(savedWarnings));
global CLASSDEF_LIFECYCLE_LOG
callerSentinel = 77;
initialStack = dbstack();
identifiers = {'Nelson:interpreter:outputNotAssigned', ...
  'Nelson:interpreter:notEnoughVarargout', 'Nelson:interpreter:invalidVarargout'};
for repetition = 1:3
  for caseIndex = 1:numel(identifiers)
    CLASSDEF_LIFECYCLE_LOG = {};
    caughtIdentifier = '';
    try
      switch caseIndex
        case 1
          [first, second] = unassigned_named_output('local');
        case 2
          [first, second] = short_varargout('local');
        case 3
          [first, second] = invalid_varargout('local');
      end
    catch exception
      caughtIdentifier = exception.identifier;
    end
    assert_isequal(caughtIdentifier, identifiers{caseIndex});
    % Output validation happens after the body, but must release its locals
    % before the caller catches the error, just like an error in the body.
    assert_isequal(CLASSDEF_LIFECYCLE_LOG, {'local:0'});
    assert_isequal(callerSentinel, 77);
    currentStack = dbstack();
    assert_isequal(numel(currentStack), numel(initialStack));
  end
end

CLASSDEF_LIFECYCLE_LOG = {};
warning('aserror', 'Nelson:classdef:destructorError');
caughtIdentifier = '';
try
  [first, second] = output_with_failing_destructor('cleanup');
catch exception
  caughtIdentifier = exception.identifier;
end
assert_isequal(caughtIdentifier, 'Nelson:interpreter:outputNotAssigned');
assert_isequal(CLASSDEF_LIFECYCLE_LOG, {'throws:cleanup'});
warning(savedWarnings);
clear global CLASSDEF_LIFECYCLE_LOG;
%=============================================================================
function [first, second] = unassigned_named_output(name)
  object = ClassdefLifecycle(name);
  first = 42;
end
%=============================================================================
function varargout = short_varargout(name)
  object = ClassdefLifecycle(name);
  varargout = {42};
end
%=============================================================================
function varargout = invalid_varargout(name)
  object = ClassdefLifecycle(name);
  varargout = 42;
end
%=============================================================================
function [first, second] = output_with_failing_destructor(name)
  object = ClassdefLifecycleThrows(name);
  first = 42;
end
%=============================================================================
