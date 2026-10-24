%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Download one public map image to the temporary directory.
date = '1985-07-04';
filename = [tempdir(), 'earth.jpg'];
url = 'https://neo.gsfc.nasa.gov/wms/wms';
try
  destinationFilename = websave(filename, url, 'Time', date, 'Service', 'WMS', ...
    'Layers', 'AVHRR_CLIM_W', 'CRS', 'CRS:84', 'Format', 'image/jpeg', ...
    'Height', '384', 'Width', '512', 'BBOX', '-180.0,-90.0,180.0,90.0', ...
    'Version', '1.3.0', 'Request', 'GetMap');
catch exception
  error(['This example requires network access to the public map service: ', exception.message]);
end
disp(['Downloaded map: ', destinationFilename]);
%=============================================================================
