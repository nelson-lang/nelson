%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU Lesser General Public
% License as published by the Free Software Foundation; either
% version 2.1 of the License, or (at your option) any later version.
%
% Alternatively, you can redistribute it and/or
% modify it under the terms of the GNU General Public License as
% published by the Free Software Foundation; either version 2 of
% the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU Lesser General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public
% License along with this program. If not, see <http://www.gnu.org/licenses/>.
% LICENCE_BLOCK_END
%=============================================================================
function res = typeofbin(code_type)
	switch(code_type)
		case 201
			res = 'sparsedouble';
		case 204
			res = 'sparselogical';
		case 104
			res = 'ndarraylogical';
		case 181
			res = 'ndarrayuint8';
		case 182
			res = 'ndarrayint8';
		case 183
			res = 'ndarrayuint16';
		case 184
			res = 'ndarrayint16';
		case 185
			res = 'ndarrayuint32';
		case 186
			res = 'ndarrayint32';
		case 187
			res = 'ndarrayuint64';
		case 188
			res = 'ndarrayint64';
		case 102
			res = 'ndarraysingle';
		case 101
			res = 'ndarraydouble';
		case 110
			res = 'ndarraystring';
		case 117
			res = 'ndarraycell';
		case 115
			res = 'ndarraystruct';
		case 18
			res = 'string';
		case 17
			res = 'cell';
		case 15
			res = 'struct';
		case 4
			res = 'logical';
		case 81
			res = 'uint8';
		case 82
			res = 'int8';
		case 83
			res = 'uint16';
		case 84
			res = 'int16';
		case 85
			res = 'uint32';
		case 86
			res = 'int32';
		case 87
			res = 'uint64';
		case 88
			res = 'int64';
		case 2
			res = 'single';
		case 1
			res = 'double';
		case 10
			res = 'char';
		case 14
			res = 'function_handle';
		case 9
			res = 'handle';
		case 1000
			res = 'generic';
		case 2000
			res = 'class';
	end
end

