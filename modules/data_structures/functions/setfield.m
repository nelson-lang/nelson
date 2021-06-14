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
function f = setfield(s, varargin)
    if ~(isstruct(s) || (iscell(s) && isempty(s)) || isempty(s))
        error(_('Invalid Input struct expected.'));
    end
    if isempty(varargin)
        error(_('Wrong numbers of input arguments.'));
    end
    f = s;
    for i = 1:2:length(varargin)
        fieldname = varargin{i};
        if isstring(fieldname) && isscalar(fieldname)
            fieldname = convertStringsToChars(fieldname);
        end
        if ~ischar(fieldname)
            error(_('Input should be a string or character array.'));
        end
        fieldvalue = varargin{i + 1};
        f.(fieldname) = fieldvalue;
    end
end
%=============================================================================
