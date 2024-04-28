%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
filename = [tempdir(), 'sunspots_annual.txt'];
api = 'http://www.ngdc.noaa.gov/stp/space-weather/';
url = [api 'solar-data/solar-indices/sunspot-numbers/' 'american/lists/list_aavso-arssn_yearly.txt'];
options = weboptions('Timeout', 120);
try
  destination_filename = websave(filename, url, options);
catch ex
  R = strcmp(ex.message, _('Forbidden (403)')) || ...
      strcmp(ex.message, _('Timeout was reached')) || ... 
      strcmp(ex.message, _('Couldn''t resolve host name'));
  skip_testsuite(R, ex.message)
end
info = dir(destination_filename);
assert_istrue(info.bytes > 1000);
txt = fileread(destination_filename);
assert_istrue(contains(txt, 'American'));
%=============================================================================
