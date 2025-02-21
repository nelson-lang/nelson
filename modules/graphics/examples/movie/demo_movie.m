%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear('M');
movie_directory = fileparts(mfilename('fullpathext'));
figure('Position', [0   50   806   530]);

% Define sequences and number of frames
sequences = {'dance', 8; 'leap', 9; 'run', 8};
nb_frames = sum([sequences{:,2}]);

% Initialize the structure M
M(nb_frames) = struct('cdata', [], 'colormap', []);

L = 1;
for s = 1:size(sequences, 1)
    action = sequences{s, 1};
    nb_frames_action = sequences{s, 2};
    
    for i = 1:nb_frames_action
        % Construct the filename for the current frame
        filename = fullfile(movie_directory, sprintf('%s_%d.png', action, i));
        
        % Read the image and store it in the movie structure
        M(L).cdata = imread(filename);
        L = L + 1;
    end
end

% Play the movie 5 times
movie(M, 5);
