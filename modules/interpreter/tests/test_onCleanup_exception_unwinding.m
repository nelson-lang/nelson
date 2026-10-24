%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
check_cleanup_exception_unwinding();
%=============================================================================
function check_cleanup_exception_unwinding()
  callerSentinel = 42;
  initialStack = dbstack();
  for failBody = [false, true]
    for repetition = 1:20
      caughtIdentifier = '';
      output = evalc('try; failing_cleanup_scope(failBody); catch ex; caughtIdentifier = ex.identifier; end');
      if failBody
        assert_isequal(caughtIdentifier, 'Nelson:test:onCleanupBody');
      else
        assert_isequal(caughtIdentifier, 'Nelson:test:onCleanupFirst');
      end
      assert_isequal(output, sprintf('remaining cleanup\n'));
      assert_isequal(callerSentinel, 42);
      currentStack = dbstack();
      assert_isequal(length(currentStack), length(initialStack));
      assert_isequal(ordinary_function(repetition), repetition + 1);
    end
  end
end
%=============================================================================
function failing_cleanup_scope(failBody)
  callerSentinel = -1;
  first = onCleanup(@() error('Nelson:test:onCleanupFirst', 'First cleanup failed.'));
  second = onCleanup(@() error('Nelson:test:onCleanupSecond', 'Second cleanup failed.'));
  remaining = onCleanup(@() disp('remaining cleanup'));
  if failBody
    error('Nelson:test:onCleanupBody', 'Function body failed.');
  end
end
%=============================================================================
function result = ordinary_function(value)
  result = value + 1;
end
%=============================================================================
