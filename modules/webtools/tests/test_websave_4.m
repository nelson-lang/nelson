%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
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
