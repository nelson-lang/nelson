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
url = 'https://neo.gsfc.nasa.gov/wms/wms';
filename = [tempdir(), 'earth3.jpg'];
options = weboptions('Timeout', Inf);
try
    destination_filename = websave(filename, url, 'Time','2004-06-01', 'Service','WMS','Layers','BlueMarbleNG-TB','CRS','CRS:84', 'Format','image/jpeg','Height', 768,'Width',1024,  'BBOX','-180.0,-90.0,180.0,90.0','Version','1.3.0','Request','GetMap', options);
    info = dir(destination_filename);
    assert_istrue(info.bytes > 290000);
catch ex
    R = strcmp(ex.message, _('Forbidden (403)')) || strcmp(ex.message, _('Timeout was reached'));
    assert_istrue(R);
end
%=============================================================================
