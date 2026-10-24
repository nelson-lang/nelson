%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('graphics_io'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
requiredTokens = {'png', 'jpg', 'jpeg', 'jfif', 'gif', 'webp', 'tiff', 'tif', ...
                  'bmp', 'dib', 'tga', 'pbm', 'pgm', 'ppm', 'pnm', 'pcx', ...
                  'psd', 'hdr', 'rgbe', 'pic'};
for page = {'imformats', 'imread', 'imwrite', 'saveas'}
  en = fileread([modulepath('graphics_io'), '/help/en_US/xml/', page{1}, '.xml']);
  fr = fileread([modulepath('graphics_io'), '/help/fr_FR/xml/', page{1}, '.xml']);
  for tokenIndex = 1:length(requiredTokens)
    token = requiredTokens{tokenIndex};
    asserts.isequal(contains(en, token), contains(fr, token));
  end
end
%=============================================================================
