%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function siogetvariable(variable_name)
  res = struct();
  res.name = variable_name;
  res.exists = isvar('base', variable_name);
  if (res.exists)
    value = acquirevar('base', variable_name);
    res.type = class(value);
    res.dims = size(value);
    if issparse(value)
      [I, J, V] = IJV(value);
      res.I = I;
      res.J = J;
      res.V = V;
    else
      if isnumeric(value)
        if isreal(value)
          res.value = value;
        else
          res.real = real(value);
          res.imag = imag(value);
        end
      else
        res.value = value;
      end
    end
  else
    res.value = [];
  end
  sioemit('send_variable', jsonencode(res));
end
%=============================================================================
