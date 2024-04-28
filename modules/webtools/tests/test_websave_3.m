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

R = strcmp(ex.message, _('Forbidden (403)')) || ...
strcmp(ex.message, _('Timeout was reached')) || ... 
strcmp(ex.message, _('Couldn''t resolve host name'));
skip_testsuite(R, ex.message)

assert_istrue(testPass)
%=============================================================================
