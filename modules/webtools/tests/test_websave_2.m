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
options = weboptions('Timeout',Inf);
destination_filename = websave(filename, url, options);
info = dir(destination_filename);
assert_istrue(info.bytes > 1000);
txt = fileread(destination_filename);
assert_istrue(contains(txt, 'American'));
%=============================================================================
