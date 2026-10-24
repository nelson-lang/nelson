%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [page, found] = externalHelpPage(keyword)
    % Read only loaded external module pages; no persistent/global help index.
    page = struct();
    found = false;
    if isstring(keyword) && isscalar(keyword)
        keyword = char(keyword);
    end
    if ~ischar(keyword) || ~isrow(keyword) || isempty(keyword)
        return
    end
    [names, paths] = getmodules('reverse');
    languages = unique({getlanguage(), getdefaultlanguage()}, 'stable');
    stylesheet = fullfile(modulepath('help_tools'), 'resources', 'nelson_json.xslt');
    for moduleIndex = 1:numel(names)
        corePath = fullfile(nelsonroot(), 'modules', names{moduleIndex});
        if samePath(paths{moduleIndex}, corePath)
            continue
        end
        for languageIndex = 1:numel(languages)
            directory = fullfile(paths{moduleIndex}, 'help', languages{languageIndex}, 'xml');
            if ~isdir(directory)
                continue
            end
            [page, found] = findPage(directory, keyword, stylesheet);
            if found
                return
            end
        end
    end
end
%=============================================================================
function [page, found] = findPage(directory, keyword, stylesheet)
    page = struct();
    found = false;
    files = dir(fullfile(directory, '*.xml'), '-s');
    if isempty(files)
        return
    end
    names = {files.name};
    preferred = strcmp(names, [keyword, '.xml']) | strcmp(names, [strrep(keyword, '.', '_'), '.xml']);
    order = [find(preferred), find(~preferred)];
    for fileIndex = order
        if strcmp(files(fileIndex).name, 'chapter.xml')
            continue
        end
        filename = fullfile(files(fileIndex).folder, files(fileIndex).name);
        try
            document = xmlread(filename, 'AllowDoctype', false);
            root = document.getDocumentElement();
            if ~strcmp(root.getTagName(), 'xmldoc')
                continue
            end
            basename = files(fileIndex).name(1:end-4);
            if ~strcmp(keyword, basename) && ~hasKeyword(root, keyword)
                continue
            end
            page = jsondecode(xslt(document, stylesheet, '-tostring'));
            fields = fieldnames(page);
            page = renameStructField(page, fields, lower(fields));
            page.examples = examples(root);
            found = true;
            return
        catch
            % A malformed/unreadable page must not prevent other help fallbacks.
            page = struct();
        end
    end
end
%=============================================================================
function matched = hasKeyword(root, keyword)
    matched = false;
    for tag = {'keyword', 'keyword_alias'}
        nodes = root.getElementsByTagName(tag{1});
        for index = 0:nodes.getLength()-1
            if strcmp(strtrim(char(nodes.item(index).getTextContent())), keyword)
                matched = true;
                return
            end
        end
    end
end
%=============================================================================
function values = examples(root)
    nodes = root.getElementsByTagName('example_item_data');
    values = cell(1, nodes.getLength());
    for index = 1:numel(values)
        values{index} = char(nodes.item(index-1).getTextContent());
    end
end
%=============================================================================
function equal = samePath(first, second)
    first = strrep(char(first), '\', '/');
    second = strrep(char(second), '\', '/');
    while endsWith(first, '/')
        first(end) = [];
    end
    while endsWith(second, '/')
        second(end) = [];
    end
    if ispc()
        equal = strcmpi(first, second);
    else
        equal = strcmp(first, second);
    end
end
