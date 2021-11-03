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
url = 'http://neowms.sci.gsfc.nasa.gov/wms/wms';
filename = [tempdir(), 'earth2.jpg'];
testPass = false;
i = 0;
retry = true;
while (retry)
  try
    destination_filename = websave(filename, url, 'Time', '2019-06-01', 'Service', 'WMS', 'Layers', 'BlueMarbleNG-TB', 'CRS', 'CRS:84', 'Format', 'image/jpeg', 'Height',768, 'Width', 1024,'BBOX','-180.0,-90.0,180.0,90.0','Version','1.3.0','Request','GetMap');
  catch ex
    testPass = (strcmp(ex.message, 'Bad Request (400)') == 1);
    if ~testPass
      testPass = (strcmp(ex.message, _('Timeout was reached')) == 1);
    end
  end
  i = i + 1;
  retry = ~testPass && (i < 5);
end
assert_istrue(testPass)
%=============================================================================
