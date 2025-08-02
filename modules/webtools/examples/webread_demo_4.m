%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
width = 1024;
height = 768;
%=============================================================================
minimum_age = 35;
gender = 'female';
%=============================================================================
for i = 1:1000
  info = webread('https://fakeface.rest/face/json', 'gender', gender, 'minimum_age', minimum_age)
  result_filename = websave([tempdir(), info.filename], info.image_url);
  html_line = ['<img src="', result_filename, '" width="', int2str(width), '" height="', int2str(height), '">'];
  clc
  inserthtml(html_line)
end

