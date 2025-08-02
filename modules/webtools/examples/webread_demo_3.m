%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% read data from National Agricultural Statistics Service
url = 'https://quickstats.nass.usda.gov/api/api_GET/?key=1C757E50-5169-30CC-BEFD-40A5C3E2A43D&format=JSON&or_desc=CROPS&domain_desc=TOTAL&agg_level_desc=COUNTY&state_name=ALABAMA&county_name=AUTAUGA&year=2012';
options = weboptions('UserAgent', 'http://www.whoishostingthis.com/tools/user-agent/');
s = webread(url, options);
s.data
%=============================================================================
