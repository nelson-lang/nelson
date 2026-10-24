%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('date'), 0);
asserts.isequal(nargout('date'), 1);
%=============================================================================
d = date();
asserts.isequal(count(d, '-'), 2);
V = datevec(datenum(d));
if V(3) > 9
  asserts.istrue(startsWith(d, num2str(V(3))));
else
  asserts.istrue(startsWith(d, ['0', num2str(V(3))]));
end
asserts.istrue(endsWith(d, num2str(V(1))));
%=============================================================================
