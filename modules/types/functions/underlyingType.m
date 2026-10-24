%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function t = underlyingType(x)
  narginchk(1, 1);
  nargoutchk(0, 1);
  t = class(x);
  % For an enumeration built on a fundamental type, the underlying type is that
  % fundamental type (e.g. an enumeration deriving from int32 -> 'int32').
  if isenum(x)
    builtinTypes = {'int8', 'int16', 'int32', 'int64', 'uint8', 'uint16', ...
      'uint32', 'uint64', 'single', 'double', 'logical', 'char'};
    try
      mc = metaclass(x);
      supers = mc.SuperclassList;
      for k = 1:numel(supers)
        name = supers(k).Name;
        if any(strcmp(name, builtinTypes))
          t = name;
          return;
        end
      end
    catch
    end
  end
end
%=============================================================================
