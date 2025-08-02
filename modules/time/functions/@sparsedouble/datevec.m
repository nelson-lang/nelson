%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = datevec(M)
  if nargin ~= 1
    error(_('Wrong number of input arguments.'));
  end
  [I, J, V] = IJV(M);
  if nargout == 1
    varargout{1} = datevec(V);
  else
    R = datevec(V);
    varargout{1} = reshape(R(:, 1), 1, []);
    varargout{2} = reshape(R(:, 2), 1, []);
    varargout{3} = reshape(R(:, 3), 1, []);
    varargout{4} = reshape(R(:, 4), 1, []);
    varargout{5} = reshape(R(:, 5), 1, []);
    varargout{6} = reshape(R(:, 6), 1, []);
  end
end
%=============================================================================
