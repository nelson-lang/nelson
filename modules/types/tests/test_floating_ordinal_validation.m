%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Floating ordinal validation must have the same errors with signed 32-bit
% and unsigned 64-bit indices, before any conversion can overflow.
A = reshape(1:4, 2, 2);
original = A;
msg_invalid = _('index must either be real positive integers or logicals.');
id_invalid = 'Nelson:types:invalidIndex:indexEitherRealPositiveIntegersLogicals';
msg_zero = _('Zero or negative index encountered.');
id_zero = 'Nelson:types:invalidIndex:zeroNegativeIndexEncountered';
invalid_values = [-1, -1.5, -realmax(), 0.5, 1.5, NaN, Inf, -Inf, 2 ^ 64];
for precision = {'double', 'single'}
  for value = invalid_values
    idx = feval(precision{1}, value);
    check_invalid_index(idx, msg_invalid, id_invalid);
  end
  idx = feval(precision{1}, 0);
  check_invalid_index(idx, msg_zero, id_zero);
  idx = feval(precision{1}, 2);
  assert_isequal(A(1, idx), 3);
  A(idx) = 8;
  assert_isequal(A, [1, 3; 8, 4]);
  A = original;
end
%=============================================================================
function check_invalid_index(idx, expected_message, expected_identifier)
  A = reshape(1:4, 2, 2);
  original = A;
  for operation = 1:4
    caught = false;
    try
      switch operation
        case 1
          A(idx) = 9;
        case 2
          A(1, idx) = 9;
        case 3
          result = A(1, idx);
        case 4
          A([1, idx]) = 9;
      end
    catch ex
      caught = true;
      assert_isequal(ex.identifier, expected_identifier);
      assert_isequal(ex.message, expected_message);
    end
    if ~caught
      error('Nelson:test:missingIndexError', sprintf('Index %g was accepted by operation %d.', idx, operation));
    end
    assert_isequal(A, original);
  end
end
%=============================================================================
