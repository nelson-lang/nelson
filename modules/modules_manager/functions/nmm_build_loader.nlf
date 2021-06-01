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
function nmm_build_loader(MODULE_NAME, MODULE_PATH)
    filewrite([MODULE_PATH, '/loader.nls'], content(MODULE_NAME));
end
%=============================================================================
function txt = content(MODULE_NAME)
    txt = ["%=============================================================================";
    "% Copyright (c) 2018-present Allan CORNET (Nelson)";
    "%";
    "% This file is released under the 3-clause BSD license. See COPYING-BSD.";
    "%=============================================================================";
    "% generated file by nmm_build_loader";
    "%=============================================================================";
    "addmodule(fileparts(nfilename('fullpath')), '" + MODULE_NAME + "');";
    "%============================================================================="];
end
