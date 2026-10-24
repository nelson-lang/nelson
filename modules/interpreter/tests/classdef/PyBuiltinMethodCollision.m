%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef PyBuiltinMethodCollision < handle
  properties
    P
  end
  methods
    function obj = PyBuiltinMethodCollision()
      obj.P = pyrun("import numpy as np; a = np.asfortranarray(np.array([0.25, 0.25]))", "a");
    end
    function value = dotReference(obj, name)
      switch name
        case 'a' % builtin double() on a py.int
          value = double(obj.P.ndim);
        case 'b' % method double() on the numpy array
          value = obj.P.double();
        case 'c' % builtin char() on a py.str
          value = char(obj.P.dtype.kind);
        case 'd' % builtin double() THEN method double() in one frame
          nd = double(obj.P.ndim);
          value = obj.P.double();
        case 'e' % builtin char() THEN method double() in one frame
          k = char(obj.P.dtype.kind);
          value = obj.P.double();
        otherwise
          error('Unexpected dotReference name.');
      end
    end
  end
end
%=============================================================================
