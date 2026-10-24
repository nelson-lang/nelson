%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
s = RandStream.create('mrg32k3a', 'Seed', 7);
x = randn(s, 2, 2);
reset(s, 7);
asserts.isequal(randn(s, 2, 2), x);
%=============================================================================
indexedStreams = RandStream.create('mrg32k3a', 'Seed', 7, ...
  'NumStreams', 4, 'StreamIndices', [2 4], 'CellOutput', true);
asserts.isequal(length(indexedStreams), 2);
asserts.isequal(indexedStreams{1}.NumStreams, 4);
asserts.isequal(indexedStreams{1}.StreamIndex, 2);
asserts.isequal(indexedStreams{1}.Substream, 1);
asserts.isequal(indexedStreams{2}.NumStreams, 4);
asserts.isequal(indexedStreams{2}.StreamIndex, 4);
asserts.isequal(indexedStreams{2}.Substream, 1);
indexedStream = RandStream.create('mrg32k3a', 'Seed', 7, ...
  'NumStreams', 4, 'StreamIndices', [2 4]);
asserts.isequal(class(indexedStream), 'RandStream');
asserts.isequal(indexedStream.StreamIndex, 2);
%=============================================================================
streams = RandStream.create('mt19937ar', 'NumStreams', 2, 'CellOutput', true);
asserts.isequal(class(streams), 'cell');
asserts.isequal(length(streams), 2);
asserts.isequal(class(streams{1}), 'RandStream');
[s1, s2] = RandStream.create('mt19937ar', 'NumStreams', 2);
asserts.isequal(s1.StreamIndex, 1);
asserts.isequal(s2.StreamIndex, 2);
asserts.checkerror('[s1, s2, s3] = RandStream.create(''mt19937ar'', ''NumStreams'', 2);', ...
  _('Too many output arguments.'));
%=============================================================================
s = RandStream('mt19937ar', 'Seed', 4);
saved = rng(s);
cleanupRng = onCleanup(@() rng(saved));
globalValues = rand(1, 3);
rng(saved);
clear cleanupRng;
reset(s, 4);
streamValues = rand(s, 1, 3);
asserts.isequal(globalValues, streamValues);
%=============================================================================
globalBackup = RandStream.getGlobalStream();
cleanupGlobal = onCleanup(@() RandStream.setGlobalStream(globalBackup));
s = RandStream('mt19937ar', 'Seed', 4);
previous = RandStream.setGlobalStream(s);
stateBefore = s.State;
rand(1, 3);
stateAfter = s.State;
asserts.isequal(class(previous), 'RandStream');
asserts.isfalse(isequal(stateBefore, stateAfter));
sameGlobal = RandStream.getGlobalStream();
asserts.istrue(sameGlobal == s);
RandStream.setGlobalStream(globalBackup);
clear cleanupGlobal;
%=============================================================================
rp = randperm(RandStream('mt19937ar', 'Seed', 9), 6, 3);
asserts.isequal(size(rp), [1 3]);
asserts.isequal(length(unique(rp)), 3);
%=============================================================================
