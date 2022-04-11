%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = cell_vertcat_struct(varargin)
	if (nargin() == 2)
		if (isempty(varargin{1}))
			r = varargin{2};
		else
			error(_('vertcat does not support cell-struct.'));
		end
	else
			error(_('Wrong number of input arguments.'));
	end
end
%=============================================================================
