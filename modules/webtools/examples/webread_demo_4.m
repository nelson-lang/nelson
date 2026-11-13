%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Inspect one JSON response and save the image it references in the temporary directory.
options = weboptions('Timeout', 15);
try
  info = webread('https://picsum.photos/id/1025/info', options);
  % The full-size download_url is several megabytes: request a 640 px wide copy.
  imageUrl = sprintf('https://picsum.photos/id/%s/640/%d', info.id, ...
    round(640 * info.height / info.width));
  destination = fullfile(tempdir(), ['picsum_', info.id, '.jpg']);
  resultFilename = websave(destination, imageUrl, options);
  fprintf('Photo %s by %s (%d x %d), page: %s\n', info.id, info.author, info.width, info.height, info.url);
catch picsumError
  % Same idea from a second public service: JSON metadata, then the image it names.
  try
    org = webread('https://api.github.com/orgs/nelson-lang', options);
    destination = fullfile(tempdir(), 'nelson-lang_avatar.png');
    resultFilename = websave(destination, [org.avatar_url, '&s=256'], options);
    fprintf('Image service unreachable (%s); used the GitHub avatar of %s instead.\n', ...
      picsumError.message, org.login);
  catch exception
    error(['This example requires network access to a public image service: ', exception.message]);
  end
end
disp(['Downloaded image: ', resultFilename]);
