%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = sparsedouble_isequaln(a, b)
	if issparse(b)
		if ~isequal(size(a), size(b))
			r = false;
		else
			[IA, JA, VA] = IJV(a);
			[IB, JB, VB] = IJV(b);
			r = isequal(IA, IB) && isequal(JA, JB) && isequaln(VA, VB);
		end
	else
		r = false;
	end
end
