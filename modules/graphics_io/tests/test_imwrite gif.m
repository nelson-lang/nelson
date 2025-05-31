%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
imfmt = imformats('gif');
msg = _('Test imwrite with GIF format');
skip_testsuite(isempty(imfmt) || (~imfmt.multipage), msg);
%=============================================================================
movie_directory = [modulepath('graphics'), '/examples/', 'movie/'];
sequences = {'dance', 8; 'leap', 9};
frameIdx = 0;
filename_gif = [tempdir, 'gif_animation.gif'];
for s = 1:size(sequences, 1)
    action = sequences{s, 1};
    nb_frames_action = sequences{s, 2};
    
    for i = 1:nb_frames_action
        % Construct the filename for the current frame
        filename = fullfile(movie_directory, sprintf('%s_%d.png', action, i));
        
        % Read the image and store it in the movie structure
        [image, map] = imread(filename);
        % Read the image
        [A, map] = imread(filename);
        if frameIdx == 1
            imwrite(A,map,filename_gif,"gif", 'LoopCount', Inf, 'DelayTime', 1);
        else
            imwrite(A,map,filename_gif,"gif", 'WriteMode', "append", 'DelayTime', 1)
        end
        frameIdx = frameIdx + 1;
    end
end
assert_istrue(isfile(filename_gif));
%=============================================================================
