//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
// The examples gallery: one framework-free component shared by every host.
// The Nelson desktop shows it in its own WebUI window (examples.js), the web
// desktop docks it as a panel and NFlow slides it in as a sheet. Hosts only
// provide the catalog loader and the actions; the markup, search, category
// rail and running states live here.
//=============================================================================
const STRINGS = {
  en_US: {
    Examples: 'Examples',
    'Search examples': 'Search examples',
    Refresh: 'Refresh',
    Close: 'Close',
    All: 'All',
    Nelson: 'Nelson',
    NFlow: 'NFlow',
    'External libraries': 'External libraries',
    Open: 'Open',
    'Open in editor': 'Open in editor',
    Run: 'Run',
    'Running…': 'Running…',
    Model: 'Model',
    Script: 'Script',
    Command: 'Command',
    'Loading examples…': 'Loading examples…',
    'No examples found.': 'No examples found.',
    'No example matches your search.': 'No example matches your search.',
    '{count} examples': '{count} examples',
    'Catalog diagnostics': 'Catalog diagnostics',
    '{count} blocks': '{count} blocks',
  },
  fr_FR: {
    Examples: 'Exemples',
    'Search examples': 'Rechercher des exemples',
    Refresh: 'Actualiser',
    Close: 'Fermer',
    All: 'Tous',
    Nelson: 'Nelson',
    NFlow: 'NFlow',
    'External libraries': 'Bibliothèques externes',
    Open: 'Ouvrir',
    'Open in editor': 'Ouvrir dans l’éditeur',
    Run: 'Exécuter',
    'Running…': 'Exécution…',
    Model: 'Modèle',
    Script: 'Script',
    Command: 'Commande',
    'Loading examples…': 'Chargement des exemples…',
    'No examples found.': 'Aucun exemple trouvé.',
    'No example matches your search.': 'Aucun exemple ne correspond à votre recherche.',
    '{count} examples': '{count} exemples',
    'Catalog diagnostics': 'Diagnostics du catalogue',
    '{count} blocks': '{count} blocs',
  },
};
const SOURCE_ORDER = { nelson: ['nelson', 'nflow', 'external'], nflow: ['nflow', 'nelson', 'external'] };
const SOURCE_HEADING = { nelson: 'Nelson', nflow: 'NFlow', external: 'External libraries' };
const RUNNING_MINIMUM_MS = 1400;
const COMPACT_WIDTH = 640;

function element(name, className, text) {
  const node = document.createElement(name);
  if (className) node.className = className;
  if (text !== undefined) node.textContent = text;
  return node;
}

function normalize(value) {
  return String(value).normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase();
}

function translator(options, locale) {
  const table = STRINGS[locale] || STRINGS.en_US;
  const custom = typeof options.translate === 'function' ? options.translate : null;
  return (key, params) => {
    let text = custom ? custom(key) : '';
    if (!text || text === key) text = table[key] || STRINGS.en_US[key] || key;
    if (params) {
      Object.keys(params).forEach((name) => {
        text = text.replace(new RegExp(`\\{${name}\\}`, 'g'), String(params[name]));
      });
    }
    return text;
  };
}

function emptyCatalog(locale) {
  return { groups: [], diagnostics: [], locale: locale || 'en_US' };
}

function normalizeCatalog(raw, fallbackLocale) {
  const catalog = emptyCatalog(fallbackLocale);
  if (!raw || typeof raw !== 'object') return catalog;
  if (typeof raw.locale === 'string' && raw.locale) catalog.locale = raw.locale;
  catalog.diagnostics = Array.isArray(raw.diagnostics) ? raw.diagnostics : [];
  catalog.groups = (Array.isArray(raw.groups) ? raw.groups : []).map((group) => ({
    name: String(group.name || ''),
    title: String(group.title || group.name || ''),
    source: group.source === 'nflow' || group.source === 'external' ? group.source : 'nelson',
    examples: (Array.isArray(group.examples) ? group.examples : []).map((example) => ({
      ...example,
      title: String(example.title || ''),
      description: String(example.description || ''),
      kind: example.kind === 'model' || example.kind === 'command' ? example.kind : 'script',
      tags: Array.isArray(example.tags) ? example.tags.map(String) : [],
      requirements: Array.isArray(example.requirements) ? example.requirements.map(String) : [],
    })),
  }));
  return catalog;
}

export function mountExampleGallery(root, options) {
  const settings = { primarySource: 'nelson', hasEditor: false, compact: 'auto', ...options };
  const state = {
    catalog: emptyCatalog(settings.locale),
    category: 'all',
    query: '',
    loading: true,
    destroyed: false,
    thumbnails: new Map(),
  };
  let t = translator(settings, state.catalog.locale);

  root.replaceChildren();
  const gallery = element('div', 'example-gallery');
  const head = element('div', 'example-gallery-head');
  const title = element('h2', 'example-gallery-title');
  const search = element('input', 'example-gallery-search');
  search.type = 'search';
  search.autocomplete = 'off';
  const refresh = element('button', 'example-gallery-icon example-gallery-refresh', '↻');
  refresh.type = 'button';
  head.append(title, search, refresh);
  let close = null;
  if (typeof settings.onClose === 'function') {
    close = element('button', 'example-gallery-icon example-gallery-close', '×');
    close.type = 'button';
    close.addEventListener('click', () => settings.onClose());
    head.append(close);
  }
  const body = element('div', 'example-gallery-body');
  const rail = element('div', 'example-gallery-rail');
  const main = element('div', 'example-gallery-main');
  body.append(rail, main);
  const foot = element('div', 'example-gallery-foot');
  const count = element('span', 'example-gallery-count');
  foot.append(count);
  if (settings.footerAction) {
    const action = element('button', 'example-gallery-foot-action', settings.footerAction.label);
    action.type = 'button';
    action.addEventListener('click', () => settings.footerAction.onClick());
    foot.append(action);
  }
  const toast = element('div', 'example-gallery-toast');
  toast.setAttribute('role', 'status');
  toast.setAttribute('aria-live', 'polite');
  gallery.append(head, body, foot, toast);
  root.append(gallery);

  let toastTimer = 0;
  function showToast(message) {
    toast.textContent = message;
    toast.classList.add('visible');
    window.clearTimeout(toastTimer);
    toastTimer = window.setTimeout(() => toast.classList.remove('visible'), 2400);
  }

  function orderedGroups() {
    const order = SOURCE_ORDER[settings.primarySource] || SOURCE_ORDER.nelson;
    return order.flatMap((source) => state.catalog.groups.filter((group) => group.source === source));
  }

  function matches(example, group, query) {
    if (!query) return true;
    const text = normalize(
      [example.title, example.description, group.title, ...example.tags, ...example.requirements].join(' ')
    );
    return text.includes(query);
  }

  function railItem(label, total, key) {
    const button = element('button', `example-gallery-category${state.category === key ? ' active' : ''}`);
    button.type = 'button';
    button.dataset.category = key;
    button.append(element('span', '', label), element('strong', '', String(total)));
    button.addEventListener('click', () => {
      state.category = key;
      main.scrollTop = 0;
      render();
    });
    return button;
  }

  function renderRail(groups) {
    rail.replaceChildren();
    const total = groups.reduce((sum, group) => sum + group.examples.length, 0);
    rail.append(railItem(t('All'), total, 'all'));
    const order = SOURCE_ORDER[settings.primarySource] || SOURCE_ORDER.nelson;
    order.forEach((source) => {
      const sourceGroups = groups.filter((group) => group.source === source);
      if (!sourceGroups.length) return;
      rail.append(element('div', 'example-gallery-rail-heading', t(SOURCE_HEADING[source])));
      sourceGroups.forEach((group) => rail.append(railItem(t(group.title), group.examples.length, group.name)));
    });
  }

  function thumbnail(example) {
    const node = element('div', 'example-gallery-thumb');
    node.setAttribute('aria-hidden', 'true');
    const fallback = () => {
      node.classList.add('fallback');
      node.append(element('span', '', '⧉'));
    };
    if (typeof settings.thumbnail !== 'function') {
      fallback();
      return node;
    }
    const key = example.file || example.title;
    const apply = (svg) => {
      if (state.destroyed) return;
      if (svg) node.innerHTML = svg;
      else fallback();
    };
    if (state.thumbnails.has(key)) {
      apply(state.thumbnails.get(key));
    } else {
      Promise.resolve(settings.thumbnail(example))
        .catch(() => null)
        .then((svg) => {
          state.thumbnails.set(key, svg || null);
          apply(svg || null);
        });
    }
    return node;
  }

  function runButton(example, group) {
    const button = element('button', 'example-gallery-primary', t('Run'));
    button.type = 'button';
    button.addEventListener('click', async () => {
      if (button.dataset.running === '1') return;
      // The interpreter runs the example asynchronously and gives no completion
      // signal back, so acknowledge the click with a visible running state.
      button.dataset.running = '1';
      button.classList.add('running');
      button.textContent = t('Running…');
      showToast(`${example.title} — ${t('Running…')}`);
      const started = Date.now();
      try {
        await settings.onRun(example, group);
      } catch (error) {
        showToast(error && error.message ? error.message : String(error));
      } finally {
        const remaining = RUNNING_MINIMUM_MS - (Date.now() - started);
        await new Promise((resolve) => window.setTimeout(resolve, Math.max(0, remaining)));
        button.dataset.running = '';
        button.classList.remove('running');
        button.textContent = t('Run');
      }
    });
    return button;
  }

  function openButton(example, group, label) {
    const button = element('button', example.kind === 'model' ? 'example-gallery-primary' : '', label);
    button.type = 'button';
    button.addEventListener('click', async () => {
      try {
        await settings.onOpen(example, group);
      } catch (error) {
        showToast(error && error.message ? error.message : String(error));
      }
    });
    return button;
  }

  function card(example, group) {
    const node = element('article', 'example-gallery-card');
    node.dataset.kind = example.kind;
    if (example.kind === 'model') node.append(thumbnail(example));
    const content = element('div', 'example-gallery-card-body');
    content.append(element('h4', '', example.title), element('p', 'example-gallery-description', example.description));
    const badges = element('div', 'example-gallery-badges');
    badges.append(element('span', 'example-gallery-badge kind', t(example.kind === 'model' ? 'Model' : example.kind === 'command' ? 'Command' : 'Script')));
    if (typeof example.blockCount === 'number' && example.blockCount > 0) {
      badges.append(element('span', 'example-gallery-badge', t('{count} blocks', { count: example.blockCount })));
    }
    example.tags.forEach((tag) => badges.append(element('span', 'example-gallery-badge', t(tag))));
    example.requirements.forEach((requirement) => badges.append(element('span', 'example-gallery-badge requirement', requirement)));
    const actions = element('div', 'example-gallery-actions');
    if (example.kind === 'model') {
      actions.append(openButton(example, group, t('Open')));
    } else {
      if (example.kind === 'script' && settings.hasEditor && typeof settings.onOpen === 'function') {
        actions.append(openButton(example, group, t('Open in editor')));
      }
      actions.append(runButton(example, group));
    }
    content.append(badges, actions);
    node.append(content);
    return node;
  }

  function renderDiagnostics() {
    const diagnostics = state.catalog.diagnostics;
    if (!diagnostics.length) return null;
    const node = element('div', 'example-gallery-diagnostics');
    node.append(element('strong', '', t('Catalog diagnostics')));
    diagnostics.forEach((item) => node.append(element('p', '', `${item.module}: ${item.message}`)));
    return node;
  }

  function render() {
    t = translator(settings, state.catalog.locale);
    title.textContent = t('Examples');
    search.placeholder = t('Search examples');
    refresh.title = t('Refresh');
    if (close) close.title = t('Close');
    const groups = orderedGroups();
    if (state.category !== 'all' && !groups.some((group) => group.name === state.category)) state.category = 'all';
    renderRail(groups);
    main.replaceChildren();
    const diagnostics = renderDiagnostics();
    if (diagnostics) main.append(diagnostics);
    if (state.loading) {
      main.append(element('div', 'example-gallery-empty', t('Loading examples…')));
      count.textContent = '';
      return;
    }
    const query = normalize(state.query.trim());
    let shown = 0;
    groups.forEach((group) => {
      if (state.category !== 'all' && state.category !== group.name) return;
      const examples = group.examples.filter((example) => matches(example, group, query));
      if (!examples.length) return;
      shown += examples.length;
      const section = element('section', 'example-gallery-group');
      const heading = element('h3', '', t(group.title));
      heading.append(element('span', '', String(examples.length)));
      const cards = element('div', 'example-gallery-cards');
      examples.forEach((example) => cards.append(card(example, group)));
      section.append(heading, cards);
      main.append(section);
    });
    if (!shown) {
      const total = groups.reduce((sum, group) => sum + group.examples.length, 0);
      main.append(element('div', 'example-gallery-empty', t(total ? 'No example matches your search.' : 'No examples found.')));
    }
    count.textContent = t('{count} examples', { count: shown });
  }

  async function load(refreshCatalog) {
    state.loading = true;
    render();
    try {
      state.catalog = normalizeCatalog(await settings.load({ refresh: !!refreshCatalog }), settings.locale);
    } catch (error) {
      state.catalog = emptyCatalog(settings.locale);
      state.catalog.diagnostics = [{ module: 'gallery', message: error && error.message ? error.message : String(error) }];
    }
    if (state.destroyed) return;
    state.loading = false;
    render();
  }

  search.addEventListener('input', () => {
    state.query = search.value;
    render();
  });
  refresh.addEventListener('click', () => {
    state.thumbnails.clear();
    load(true);
  });

  let observer = null;
  const applyCompact = (compact) => gallery.classList.toggle('compact', compact);
  if (settings.compact === 'auto' && typeof ResizeObserver === 'function') {
    observer = new ResizeObserver((entries) => {
      entries.forEach((entry) => applyCompact(entry.contentRect.width > 0 && entry.contentRect.width < COMPACT_WIDTH));
    });
    observer.observe(root);
  } else {
    applyCompact(settings.compact === true);
  }

  load(false);

  return {
    reload: (refreshCatalog = false) => load(refreshCatalog),
    focusSearch: () => search.focus(),
    destroy: () => {
      state.destroyed = true;
      if (observer) observer.disconnect();
      window.clearTimeout(toastTimer);
      root.replaceChildren();
    },
  };
}
