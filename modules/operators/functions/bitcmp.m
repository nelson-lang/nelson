%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = bitcmp(A, assumedtype)
  narginchk(1, 2);
  restoreDouble = false;
  if nargin == 2
    cls = char(assumedtype);
    if isinteger(A)
      if ~strcmp(class(A), cls)
        error('Nelson:bitcmp:integerInputsCannotAssumedAny2', 'Integer inputs cannot be assumed as any other types than their own types.');
      end
    else
      % assumedtype only applies to double input; the result stays double.
      restoreDouble = true;
      if ~isreal(A) || ~all(isfinite(A(:))) || ~isequal(A, floor(A)) ...
        || any(A(:) < double(intmin(cls))) || any(A(:) > double(intmax(cls)))
        error('Nelson:operators:invalidBitInput:doubleInputsIntegerValuesRangeASSUMEDTYPE', 'Double inputs must have integer values in the range of ASSUMEDTYPE.');
      end
      A = cast(A, cls);
    end
  else
    cls = class(A);
  end
  if ~isinteger(A)
    error('Nelson:bitcmp:inputsIntegersUseAssumedInteger2', 'Inputs must be integers or use an assumed integer type.');
  end
  if intmin(cls) < 0
    allOnes = cast(-1, cls);
  else
    allOnes = intmax(cls);
  end
  r = bitxor(A, allOnes);
  if restoreDouble
    r = double(r);
  end
end
%=============================================================================
