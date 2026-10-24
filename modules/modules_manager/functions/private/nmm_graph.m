%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function report = nmm_graph(varargin)
  if length(varargin) ~= 1
    nmm_error('WrongNumberOfInputArguments', 'Wrong number of input arguments.');
  end
  packageName = varargin{1};
  if ~ischar(packageName)
    nmm_error('CharactersExpected', '#%d argument: characters expected.', 2);
  end
  tree = nmm_tree(packageName);
  labels = {};
  edges = struct([]);
  [labels, edges] = collectGraph(tree, labels, edges);
  report = struct();
  report.package = packageName;
  report.dot = dotGraph(labels, edges);
  report.mermaid = mermaidGraph(labels, edges);
end
%=============================================================================
function [labels, edges] = collectGraph(node, labels, edges)
  parent = nodeLabel(node);
  labels = appendLabel(labels, parent);
  for k = 1:length(node.dependencies)
    child = nodeLabel(node.dependencies(k));
    labels = appendLabel(labels, child);
    edges = appendEdge(edges, parent, child, node.dependencies(k).requested);
    [labels, edges] = collectGraph(node.dependencies(k), labels, edges);
  end
end
%=============================================================================
function text = dotGraph(labels, edges)
  lines = {'digraph nmm {'};
  for k = 1:length(labels)
    lines{end + 1} = ['  "', escapeLabel(labels{k}), '";'];
  end
  for k = 1:length(edges)
    label = '';
    if ~isempty(edges(k).requested)
      label = [' [label="', escapeLabel(edges(k).requested), '"]'];
    end
    lines{end + 1} = ['  "', escapeLabel(edges(k).from), '" -> "', escapeLabel(edges(k).to), '"', label, ';'];
  end
  lines{end + 1} = '}';
  text = strjoin(lines, newline);
end
%=============================================================================
function text = mermaidGraph(labels, edges)
  lines = {'graph TD'};
  for k = 1:length(labels)
    lines{end + 1} = ['  n', int2str(k), '["', escapeLabel(labels{k}), '"]'];
  end
  for k = 1:length(edges)
    from = findLabel(labels, edges(k).from);
    to = findLabel(labels, edges(k).to);
    lines{end + 1} = ['  n', int2str(from), ' --> n', int2str(to)];
  end
  text = strjoin(lines, newline);
end
%=============================================================================
function label = nodeLabel(node)
  label = node.package;
  if ~isempty(node.version)
    label = [label, '@', node.version];
  elseif ~isempty(node.requested)
    label = [label, '@', node.requested];
  end
end
%=============================================================================
function labels = appendLabel(labels, label)
  if ~any(strcmp(labels, label))
    labels{end + 1} = label;
  end
end
%=============================================================================
function edges = appendEdge(edges, from, to, requested)
  edge = struct();
  edge.from = from;
  edge.to = to;
  edge.requested = requested;
  if isempty(edges)
    edges = edge;
  else
    edges(end + 1) = edge;
  end
end
%=============================================================================
function idx = findLabel(labels, label)
  idx = 1;
  for k = 1:length(labels)
    if strcmp(labels{k}, label)
      idx = k;
      return
    end
  end
end
%=============================================================================
function label = escapeLabel(label)
  label = strrep(label, '\', '\\');
  label = strrep(label, '"', '\"');
end
%=============================================================================
