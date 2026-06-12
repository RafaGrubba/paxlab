'use strict';
// PaxLab — front-end (Tailwind + htmx + JS) consumindo a API REST /api/*.
// O token Bearer fica no localStorage; toda chamada usa fetch.

const State = {
  get token() { return localStorage.getItem('paxlab_token'); },
  set token(t) { t ? localStorage.setItem('paxlab_token', t) : localStorage.removeItem('paxlab_token'); },
  get name() { return localStorage.getItem('paxlab_name') || ''; },
  set name(n) { n ? localStorage.setItem('paxlab_name', n) : localStorage.removeItem('paxlab_name'); },
};

// ---- helpers ----------------------------------------------------------------
const $app = () => document.getElementById('app');
const $nav = () => document.getElementById('nav');
const esc = (s) => String(s ?? '').replace(/[&<>"']/g, c =>
  ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));

async function api(method, path, body) {
  const headers = { 'Content-Type': 'application/json' };
  if (State.token) headers['Authorization'] = 'Bearer ' + State.token;
  const res = await fetch(path, {
    method, headers, body: body ? JSON.stringify(body) : undefined,
  });
  let data = null;
  try { data = await res.json(); } catch (_) { /* sem corpo */ }
  if (!res.ok) {
    if (res.status === 401) { logout(); }
    throw new Error((data && data.error) || ('Erro ' + res.status));
  }
  return data;
}

const btn = 'inline-flex items-center gap-1 rounded-lg px-3 py-1.5 text-sm font-medium';
const btnPrimary = btn + ' bg-emerald-600 text-white hover:bg-emerald-700';
const btnGhost = btn + ' bg-white border border-slate-300 hover:bg-slate-100';
const card = 'bg-white border border-slate-200 rounded-xl p-5 mb-4 shadow-sm';
const input = 'w-full rounded-lg border border-slate-300 px-3 py-2 mt-1';
const label = 'block text-sm font-medium mt-3';

function toast(msg, ok) {
  const d = document.createElement('div');
  d.className = 'fixed bottom-4 right-4 px-4 py-2 rounded-lg text-white shadow-lg ' +
    (ok ? 'bg-emerald-600' : 'bg-rose-600');
  d.textContent = msg;
  document.body.appendChild(d);
  setTimeout(() => d.remove(), 3000);
}

// ---- navegação --------------------------------------------------------------
function renderNav() {
  if (!State.token) { $nav().innerHTML = ''; return; }
  const link = (fn, txt) => `<button onclick="${fn}" class="text-sky-100 hover:text-white text-sm">${txt}</button>`;
  $nav().innerHTML = `
    <nav class="bg-slate-800 text-white px-4 py-3 flex items-center gap-4 flex-wrap">
      <button onclick="go('home')" class="font-bold text-lg">🧬 PaxLab</button>
      <div class="flex-1"></div>
      ${link("go('home')", 'Início')}
      ${link("go('sequences')", 'Sequências')}
      ${link("go('align')", 'Alinhar')}
      ${link("go('import')", 'Importar')}
      ${link("go('compendio')", 'Compêndio')}
      <span class="text-slate-400 text-sm">${esc(State.name)}</span>
      ${link('logout()', 'Sair')}
    </nav>`;
}

function go(view, arg) {
  if (location.hash) history.replaceState(null, '', location.pathname);
  renderNav();
  const fn = Views[view];
  if (fn) fn(arg); else Views.sequences();
  window.scrollTo(0, 0);
}

function logout() {
  State.token = null; State.name = null;
  renderNav();
  Views.login();
}

// ---- Views ------------------------------------------------------------------
const Views = {};

Views.login = function () {
  $nav().innerHTML = '';
  $app().innerHTML = `
    <section class="${card} max-w-md mx-auto mt-10">
      <h1 class="text-2xl font-bold">🧬 PaxLab</h1>
      <p class="text-slate-500 text-sm">Bioinformática reprodutível.</p>
      <form id="f" class="mt-4">
        <label class="${label}">E-mail</label>
        <input class="${input}" name="email" type="email" required value="demo@paxlab.bio">
        <label class="${label}">Senha</label>
        <input class="${input}" name="password" type="password" required value="paxlab123">
        <button class="${btnPrimary} mt-4 w-full justify-center">Entrar</button>
      </form>
      <p class="text-sm text-slate-500 mt-3">
        Não tem conta? <button onclick="Views.register()" class="text-sky-600">Criar conta</button>
      </p>
    </section>`;
  document.getElementById('f').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      const d = await api('POST', '/api/login',
        { email: f.email.value, password: f.password.value });
      State.token = d.token; State.name = d.name;
      go('home');
    } catch (err) { toast(err.message, false); }
  };
};

Views.register = function () {
  $nav().innerHTML = '';
  $app().innerHTML = `
    <section class="${card} max-w-md mx-auto mt-10">
      <h1 class="text-2xl font-bold">Criar conta</h1>
      <form id="f" class="mt-4">
        <label class="${label}">Nome</label>
        <input class="${input}" name="name" required>
        <label class="${label}">E-mail</label>
        <input class="${input}" name="email" type="email" required>
        <label class="${label}">Senha</label>
        <input class="${input}" name="password" type="password" required>
        <button class="${btnPrimary} mt-4 w-full justify-center">Criar conta</button>
      </form>
      <p class="text-sm text-slate-500 mt-3">
        Já tem conta? <button onclick="Views.login()" class="text-sky-600">Entrar</button>
      </p>`;
  document.getElementById('f').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      const d = await api('POST', '/api/register',
        { name: f.name.value, email: f.email.value, password: f.password.value });
      State.token = d.token; State.name = d.name;
      go('home');
    } catch (err) { toast(err.message, false); }
  };
};

Views.home = function () {
  const tool = (view, ico, ttl, txt) => `
    <button onclick="go('${view}')" class="${card} text-left hover:border-emerald-400 w-full">
      <div class="text-2xl">${ico}</div>
      <div class="font-semibold mt-1">${ttl}</div>
      <div class="text-sm text-slate-500">${txt}</div>
    </button>`;
  $app().innerHTML = `
    <section class="${card}">
      <h1 class="text-2xl font-bold">Olá, ${esc(State.name)}</h1>
      <p class="text-slate-500">Seu laboratório de bioinformática reprodutível.</p>
    </section>
    <div class="grid sm:grid-cols-2 gap-4">
      ${tool('sequences', '🧬', 'Sequências', 'Cadastre e analise DNA/RNA/proteína.')}
      ${tool('align', '🔗', 'Alinhar', 'Compare duas sequências (Needleman–Wunsch).')}
      ${tool('import', '🌐', 'Importar do NCBI', 'Traga genes reais por número de acesso.')}
      ${tool('biocifra', '🔐', 'Biocifra', 'Cifre mensagens dentro do DNA com senha.')}
      ${tool('compendio', '📖', 'Compêndio', 'Conceitos de biologia molecular.')}
      ${tool('favorites', '⭐', 'Favoritos', 'Análises e alinhamentos favoritos.')}
    </div>`;
};

// ---- Sequências -------------------------------------------------------------
Views.sequences = async function (q) {
  $app().innerHTML = `
    <section class="${card}">
      <h2 class="text-xl font-bold">Nova sequência</h2>
      <p class="text-sm text-slate-500">Cole FASTA ou resíduos crus (DNA, RNA ou proteína).</p>
      <form id="new" class="mt-2">
        <input class="${input}" name="name" placeholder="Ex.: Insulina humana (INS)" required>
        <textarea class="${input} font-mono text-sm" name="data" rows="4"
          placeholder=">minha_seq&#10;ATGGCC..." required></textarea>
        <button class="${btnPrimary} mt-3">Salvar sequência</button>
      </form>
    </section>
    <section class="${card}">
      <div class="flex items-center justify-between gap-2">
        <h2 class="text-xl font-bold">Minhas sequências</h2>
        <form id="search" class="flex gap-2">
          <input class="rounded-lg border border-slate-300 px-3 py-1.5 text-sm" name="q"
            placeholder="Buscar…" value="${esc(q || '')}">
        </form>
      </div>
      <ul id="list" class="mt-3 divide-y"></ul>
    </section>`;
  document.getElementById('new').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      await api('POST', '/api/sequences', { name: f.name.value, data: f.data.value });
      toast('Sequência salva.', true);
      Views.sequences();
    } catch (err) { toast(err.message, false); }
  };
  document.getElementById('search').onsubmit = (e) => {
    e.preventDefault();
    Views.sequences(e.target.q.value);
  };
  const url = '/api/sequences' + (q ? ('?q=' + encodeURIComponent(q)) : '');
  const seqs = await api('GET', url);
  document.getElementById('list').innerHTML = seqs.length ? seqs.map(s => `
    <li class="py-2 flex items-center gap-3">
      <button onclick="go('seq', ${s.id})" class="text-sky-600 font-medium">${esc(s.name)}</button>
      <span class="text-xs bg-sky-100 text-sky-700 rounded-full px-2 py-0.5">${esc(s.kind)}</span>
      <span class="text-xs text-slate-400">${s.length} resíduos</span>
    </li>`).join('') : '<li class="py-2 text-slate-500">Nenhuma sequência encontrada.</li>';
};

Views.seq = async function (id) {
  const s = await api('GET', '/api/sequences/' + id);
  const isProt = s.kind === 'Protein';
  $app().innerHTML = `
    <button onclick="go('sequences')" class="${btnGhost} mb-3">← Sequências</button>
    <section class="${card}">
      <div class="flex items-start justify-between gap-2">
        <h2 class="text-xl font-bold">${esc(s.name)}
          <span class="text-xs bg-sky-100 text-sky-700 rounded-full px-2 py-0.5 align-middle">${esc(s.kind)}</span>
        </h2>
        <a href="/api/sequences/${id}/fasta" class="text-sm text-sky-600">baixar FASTA</a>
      </div>
      <pre id="seqbox" class="bg-slate-900 text-emerald-200 rounded-lg p-3 mt-2 text-xs whitespace-pre-wrap break-all"></pre>
      <div id="bio" class="mt-3"></div>
    </section>
    <section class="${card}">
      <h3 class="font-semibold">Rodar análise</h3>
      <form id="an" class="flex flex-wrap items-end gap-2 mt-2">
        <select name="op" class="rounded-lg border border-slate-300 px-3 py-2">
          <option value="transcribe">Transcrição (DNA → RNA)</option>
          <option value="translate">Tradução (→ proteína)</option>
          <option value="reversecomplement">Fita complementar reversa</option>
          <option value="gccontent">Conteúdo GC</option>
          <option value="findorfs">Buscar ORFs</option>
        </select>
        <input id="minlen" name="minlen" type="number" value="30"
          class="rounded-lg border border-slate-300 px-3 py-2 w-24 hidden" title="mín. ORF (aa)">
        <button class="${btnPrimary}">Analisar</button>
      </form>
      <h3 class="font-semibold mt-4">Histórico</h3>
      <div id="hist" class="mt-2"></div>
    </section>
    <section class="${card}">
      <details>
        <summary class="cursor-pointer font-semibold text-sky-700">Editar / excluir</summary>
        <form id="edit" class="mt-2">
          <label class="${label}">Nome</label>
          <input class="${input}" name="name" value="${esc(s.name)}" required>
          <label class="${label}">Sequência</label>
          <textarea class="${input} font-mono text-sm" name="data" rows="3" required>${esc(s.residues)}</textarea>
          <button class="${btnPrimary} mt-2">Salvar alterações</button>
        </form>
        <button onclick="delSeq(${id})" class="${btn} bg-rose-600 text-white mt-3">Excluir sequência</button>
      </details>
    </section>`;

  // mostra/esconde minlen conforme a operação
  const opSel = document.querySelector('#an select[name=op]');
  const ml = document.getElementById('minlen');
  opSel.onchange = () => ml.classList.toggle('hidden', opSel.value !== 'findorfs');

  // sequência com ORFs destacadas
  let orfs = [];
  if (!isProt) {
    try { const d = await api('GET', `/api/sequences/${id}/orfs`); orfs = d.orfs || []; } catch (_) {}
  }
  document.getElementById('seqbox').innerHTML = highlightSeq(s.residues, orfs);
  renderBio(id, isProt, orfs);

  document.getElementById('an').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    const body = { op: f.op.value };
    if (f.op.value === 'findorfs') body.minlen = parseInt(f.minlen.value, 10);
    try {
      await api('POST', `/api/sequences/${id}/analyze`, body);
      loadHistory(id);
    } catch (err) { toast(err.message, false); }
  };
  document.getElementById('edit').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      await api('PUT', '/api/sequences/' + id, { name: f.name.value, data: f.data.value });
      toast('Salvo.', true); go('seq', id);
    } catch (err) { toast(err.message, false); }
  };
  _refresh = () => loadHistory(id);
  loadHistory(id);
};

function highlightSeq(res, orfs) {
  const frame = new Array(res.length).fill(0);
  orfs.forEach(o => { for (let i = o.start; i < o.end && i < res.length; i++) frame[i] = ((o.frame - 1) % 3) + 1; });
  const colors = { 1: 'bg-emerald-500/40', 2: 'bg-sky-500/40', 3: 'bg-amber-500/40' };
  let out = '';
  for (let i = 0; i < res.length; i++) {
    if (i > 0 && i % 10 === 0) out += ' ';
    const c = esc(res[i]);
    out += frame[i] ? `<span class="${colors[frame[i]]} rounded">${c}</span>` : c;
  }
  return out;
}

async function renderBio(id, isProt, orfs) {
  const box = document.getElementById('bio');
  if (isProt) {
    try {
      const p = await api('GET', `/api/sequences/${id}/protein`);
      box.innerHTML = `<h3 class="font-semibold mt-2">Propriedades da proteína</h3>
        <p class="text-sm text-slate-600">Comprimento: <b>${p.length} aa</b> · Massa: <b>${Math.round(p.weight)} Da</b></p>
        <div class="flex flex-wrap gap-1 mt-1">${(p.composition || []).slice(0, 10).map(c =>
          `<span class="text-xs bg-slate-100 rounded px-2 py-0.5">${esc(c.aa)}×${c.count}</span>`).join('')}</div>`;
    } catch (_) {}
  } else {
    let html = '';
    if (orfs.length) html += `<p class="text-xs text-slate-400">Cores na sequência = ORFs por frame (+1, +2, +3).</p>`;
    try {
      const rs = await api('GET', `/api/sequences/${id}/restriction`);
      if (rs.length) html += `<h3 class="font-semibold mt-2">Sítios de restrição</h3><ul class="text-sm">${
        rs.map(e => `<li><b>${esc(e.name)}</b> <span class="text-xs bg-slate-100 rounded px-1">${esc(e.site)}</span> — pos. ${e.positions.join(', ')}</li>`).join('')}</ul>`;
    } catch (_) {}
    box.innerHTML = html;
  }
}

async function loadHistory(id) {
  const as = await api('GET', `/api/sequences/${id}/analyses`);
  document.getElementById('hist').innerHTML = as.length ? as.map(analysisCard).join('')
    : '<p class="text-slate-500 text-sm">Ainda sem análises.</p>';
}

function analysisCard(a) {
  const link = location.origin + '/#a/' + a.token;
  return `
    <div class="border border-slate-200 rounded-lg p-3 my-2">
      <div class="flex items-center justify-between">
        <strong>${esc(a.operation)}</strong>
        <details class="relative">
          <summary class="list-none cursor-pointer px-2 text-slate-500 hover:text-slate-800 text-lg leading-none">⋮</summary>
          <div class="absolute right-0 mt-1 w-44 bg-white border border-slate-200 rounded-lg shadow-lg z-10 overflow-hidden text-sm">
            <button onclick="favAnalysis('${a.token}')" class="block w-full text-left px-3 py-2 hover:bg-slate-100">${a.favorite ? '☆ Desfavoritar' : '⭐ Favoritar'}</button>
            <button onclick="delAnalysis('${a.token}')" class="block w-full text-left px-3 py-2 hover:bg-slate-100 text-rose-600">🗑 Excluir</button>
          </div>
        </details>
      </div>
      <pre class="bg-slate-900 text-emerald-200 rounded p-2 mt-1 text-xs whitespace-pre-wrap break-all">${esc(a.resultText)}</pre>
      <p class="text-xs text-slate-500 mt-1">${esc(a.summary)}</p>
      <details class="mt-2">
        <summary class="list-none cursor-pointer text-sky-600 text-sm w-max">🔗 permalink</summary>
        <input readonly onclick="this.select()" value="${esc(link)}"
          class="w-full mt-1 rounded border border-slate-300 px-2 py-1 text-xs font-mono bg-slate-50">
      </details>
    </div>`;
}

let _refresh = () => {};
async function favAnalysis(tok) { try { await api('POST', `/api/analyses/${tok}/favorite`); _refresh(); } catch (e) { toast(e.message, false); } }
async function delAnalysis(tok) {
  if (!confirm('Excluir esta análise?')) return;
  try { await api('DELETE', '/api/analyses/' + tok); _refresh(); } catch (e) { toast(e.message, false); }
}

async function delSeq(id) {
  if (!confirm('Excluir esta sequência e suas análises?')) return;
  try { await api('DELETE', '/api/sequences/' + id); toast('Excluída.', true); go('sequences'); }
  catch (e) { toast(e.message, false); }
}

Views.permalink = async function (tok) {
  const d = await api('GET', '/api/analyses/' + tok);
  const a = d.analysis;
  $app().innerHTML = `
    ${State.token ? `<button onclick="go('home')" class="${btnGhost} mb-3">← Início</button>` : ''}
    <section class="${card}">
      <h2 class="text-xl font-bold">Análise compartilhada</h2>
      <p class="text-sm text-slate-500">Operação: <b>${esc(a.operation)}</b> · Sequência: ${esc(d.sequenceName)}</p>
      <div class="rounded-lg p-2 my-2 text-sm font-medium ${d.matches ? 'bg-emerald-100 text-emerald-800' : 'bg-rose-100 text-rose-800'}">
        ${d.matches ? '✓ Reproduzido: rodar a receita agora deu o mesmo resultado.' : '⚠ Divergência na reprodução.'}
      </div>
      <h3 class="font-semibold">Resultado</h3>
      <pre class="bg-slate-900 text-emerald-200 rounded p-2 mt-1 text-xs whitespace-pre-wrap break-all">${esc(d.reproduced.text)}</pre>
      <p class="text-xs text-slate-500 mt-1">${esc(d.reproduced.summary)}</p>
      <div class="mt-3 flex gap-2">
        <button onclick="forkAnalysis('${tok}')" class="${btnPrimary}">Forkar p/ minhas análises</button>
      </div>
    </section>`;
};
async function forkAnalysis(tok) {
  try { await api('POST', `/api/analyses/${tok}/fork`); toast('Forkado.', true); }
  catch (e) { toast(e.message, false); }
}

// ---- Alinhamento ------------------------------------------------------------
Views.align = async function () {
  const seqs = await api('GET', '/api/sequences');
  const opts = seqs.map(s => `<option value="${s.id}">${esc(s.name)}</option>`).join('');
  $app().innerHTML = `
    <section class="${card}">
      <h2 class="text-xl font-bold">Alinhamento global (Needleman–Wunsch)</h2>
      ${seqs.length < 2 ? `<p class="text-slate-500 mt-2">Você precisa de pelo menos 2 sequências cadastradas.</p>` : `
      <form id="al" class="mt-2">
        <label class="${label}">Sequência A</label>
        <select name="a" class="${input}">${opts}</select>
        <label class="${label}">Sequência B</label>
        <select name="b" class="${input}">${opts}</select>
        <label class="${label}">Pontuação</label>
        <div class="flex gap-2">
          <label class="flex-1 text-xs text-slate-500">igual<input name="match" type="number" value="1" class="${input}"></label>
          <label class="flex-1 text-xs text-slate-500">diferente<input name="mismatch" type="number" value="-1" class="${input}"></label>
          <label class="flex-1 text-xs text-slate-500">lacuna<input name="gap" type="number" value="-2" class="${input}"></label>
        </div>
        <button class="${btnPrimary} mt-3">Alinhar e registrar</button>
      </form>`}
      <div id="alres" class="mt-3"></div>
    </section>
    <section class="${card}">
      <h3 class="font-semibold">Alinhamentos registrados</h3>
      <ul id="allist" class="mt-2 divide-y"></ul>
    </section>`;
  const form = document.getElementById('al');
  if (form) {
    if (seqs.length >= 2) form.b.selectedIndex = 1;
    form.onsubmit = async (e) => {
      e.preventDefault();
      const f = e.target;
      try {
        const d = await api('POST', '/api/align', {
          seqA: +f.a.value, seqB: +f.b.value,
          match: +f.match.value, mismatch: +f.mismatch.value, gap: +f.gap.value });
        document.getElementById('alres').innerHTML = alignResult(d.record, d.alignedA, d.alignedB);
        loadAlignList();
      } catch (err) { toast(err.message, false); }
    };
  }
  loadAlignList();
};
async function loadAlignList() {
  const rs = await api('GET', '/api/alignments');
  document.getElementById('allist').innerHTML = rs.length ? rs.map(r => `
    <li class="py-2 flex items-center gap-2">
      ${r.favorite ? '<span title="favorito">⭐</span>' : ''}
      <button onclick="viewAlign('${r.token}')" class="text-sky-600 font-medium">${esc(r.seqAName)} × ${esc(r.seqBName)}</button>
      <span class="text-xs bg-slate-100 rounded px-2 py-0.5">score ${r.score}</span>
      <span class="text-xs text-slate-400">${r.identity.toFixed(1)}%</span>
      <details class="relative ml-auto">
        <summary class="list-none cursor-pointer px-2 text-slate-500 text-lg leading-none">⋮</summary>
        <div class="absolute right-0 mt-1 w-44 bg-white border rounded-lg shadow-lg z-10 text-sm overflow-hidden">
          <button onclick="favAlign('${r.token}')" class="block w-full text-left px-3 py-2 hover:bg-slate-100">${r.favorite ? '☆ Desfavoritar' : '⭐ Favoritar'}</button>
          <button onclick="delAlignF('${r.token}')" class="block w-full text-left px-3 py-2 hover:bg-slate-100 text-rose-600">🗑 Excluir</button>
        </div>
      </details>
    </li>`).join('') : '<li class="py-2 text-slate-500">Nenhum alinhamento ainda.</li>';
}
async function viewAlign(tok) {
  const res = document.getElementById('alres');
  if (!res) { location.hash = '#al/' + tok; return; }
  const d = await api('GET', '/api/alignments/' + tok);
  res.innerHTML = alignResult(d.record, d.alignedA, d.alignedB);
  window.scrollTo(0, 0);
}
function alignResult(r, a, b) {
  const link = location.origin + '/#al/' + r.token;
  return `
    <div class="border rounded-lg p-3">
      <p class="text-sm text-slate-600">${esc(r.seqAName)} × ${esc(r.seqBName)} ·
        <b>score ${r.score}</b> · <b>${r.identity.toFixed(1)}%</b> · pont. ${r.match}/${r.mismatch}/${r.gap}</p>
      <p class="text-xs text-slate-400 mt-1">verde = igual · laranja = diferente · vermelho = lacuna</p>
      <div class="overflow-x-auto mt-1">${alignViz(a, b)}</div>
      <details class="mt-2">
        <summary class="list-none cursor-pointer text-sky-600 text-sm w-max">🔗 permalink</summary>
        <input readonly onclick="this.select()" value="${esc(link)}" class="w-full mt-1 rounded border px-2 py-1 text-xs font-mono bg-slate-50">
      </details>
    </div>`;
}
function alignViz(a, b) {
  const row = (self, other) => {
    let out = '';
    for (let i = 0; i < self.length; i++) {
      const x = self[i], y = other[i];
      let cls = 'text-emerald-400';
      if (x === '-' || y === '-') cls = 'text-rose-400';
      else if (x !== y) cls = 'text-amber-400';
      out += `<span class="${cls}">${esc(x)}</span>`;
    }
    return out;
  };
  return `<pre class="bg-slate-900 rounded p-2 text-xs leading-5" style="width:max-content;min-width:100%">${row(a, b)}\n${row(b, a)}</pre>`;
}
async function favAlign(tok) { try { await api('POST', `/api/alignments/${tok}/favorite`); loadAlignList(); } catch (e) { toast(e.message, false); } }
async function delAlignF(tok) { if (!confirm('Excluir este alinhamento?')) return; try { await api('DELETE', '/api/alignments/' + tok); loadAlignList(); } catch (e) { toast(e.message, false); } }
Views.alignView = async function (tok) {
  const d = await api('GET', '/api/alignments/' + tok);
  $app().innerHTML = `
    ${State.token ? `<button onclick="go('align')" class="${btnGhost} mb-3">← Alinhamentos</button>` : ''}
    <section class="${card}"><h2 class="text-xl font-bold">Alinhamento compartilhado</h2>
      <div class="mt-2">${alignResult(d.record, d.alignedA, d.alignedB)}</div></section>`;
};

// ---- Importar NCBI ----------------------------------------------------------
Views.import = function () {
  $app().innerHTML = `
    <section class="${card} max-w-md">
      <h2 class="text-xl font-bold">Importar do NCBI</h2>
      <p class="text-sm text-slate-500">Número de acesso do GenBank (ex.: NM_000207 — insulina humana).</p>
      <form id="imp" class="mt-2">
        <input class="${input}" name="acc" placeholder="NM_000207" required>
        <button class="${btnPrimary} mt-3">Buscar e salvar</button>
      </form>
    </section>`;
  document.getElementById('imp').onsubmit = async (e) => {
    e.preventDefault();
    try {
      const s = await api('POST', '/api/import', { accession: e.target.acc.value });
      toast('Importada: ' + s.name, true);
      go('seq', s.id);
    } catch (err) { toast(err.message, false); }
  };
};

// ---- Biocifra ---------------------------------------------------------------
Views.biocifra = function () {
  $app().innerHTML = `
    <section class="${card}"><h2 class="text-xl font-bold">🔐 Biocifra</h2>
      <p class="text-sm text-slate-500">Cifre mensagens dentro de uma sequência de DNA com uma senha. Sem a senha certa, não decifra.</p></section>
    <section class="${card}">
      <h3 class="font-semibold">Cifrar texto em DNA</h3>
      <form id="enc" class="mt-2">
        <textarea class="${input} font-mono text-sm" name="text" rows="2" placeholder="Sua mensagem secreta…" required></textarea>
        <input class="${input}" name="key" placeholder="senha" required>
        <button class="${btnPrimary} mt-2">Cifrar</button>
      </form>
      <div id="encout" class="mt-2"></div>
    </section>
    <section class="${card}">
      <h3 class="font-semibold">Decifrar uma sequência</h3>
      <form id="dec" class="mt-2">
        <textarea class="${input} font-mono text-sm" name="dna" rows="2" placeholder="ACGTACGT…" required></textarea>
        <input class="${input}" name="key" placeholder="a mesma senha" required>
        <button class="${btnPrimary} mt-2">Decifrar</button>
      </form>
      <div id="decout" class="mt-2"></div>
    </section>`;
  document.getElementById('enc').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      const d = await api('POST', '/api/biocifra/encode', { text: f.text.value, key: f.key.value });
      document.getElementById('encout').innerHTML =
        `<textarea id="encdna" readonly rows="2" class="${input} font-mono text-xs">${esc(d.dna)}</textarea>
         <button onclick="copyEl('encdna', this)" class="${btnGhost} mt-1">Copiar</button>`;
    } catch (err) { toast(err.message, false); }
  };
  document.getElementById('dec').onsubmit = async (e) => {
    e.preventDefault();
    const f = e.target;
    try {
      const d = await api('POST', '/api/biocifra/decode', { dna: f.dna.value, key: f.key.value });
      document.getElementById('decout').innerHTML =
        `<pre class="bg-slate-900 text-emerald-200 rounded p-2 text-sm whitespace-pre-wrap break-all">${esc(d.text)}</pre>`;
    } catch (err) {
      document.getElementById('decout').innerHTML = `<p class="text-rose-600 text-sm">${esc(err.message)}</p>`;
    }
  };
};
function copyEl(id, btn) { navigator.clipboard.writeText(document.getElementById(id).value); btn.textContent = 'Copiado!'; }

// ---- Compêndio --------------------------------------------------------------
const COMPENDIO = [
  [
    ['DNA', 'A molécula que armazena a informação genética da célula, em fita dupla. Alfabeto de quatro bases — A, C, G e T; é a ordem delas que carrega a informação. As fitas são complementares: A pareia com T, C com G.'],
    ['RNA', 'Uma cópia de trabalho de um trecho do DNA, em geral de fita simples. Usa U no lugar de T; o RNA mensageiro leva a receita do gene até o ribossomo.'],
    ['Proteína', 'O que executa o trabalho na célula (enzimas, estrutura, sinalização). Cadeia de aminoácidos (20 tipos, cada um com uma letra); a ordem deles determina como ela se dobra e funciona.'],
    ['Códon', 'Uma trinca de bases. Com 4 bases há 4³ = 64 códons, e cada um corresponde a um aminoácido ou a um sinal de parada. É a unidade de leitura do código genético.'],
    ['Código genético', 'A tabela que diz qual aminoácido cada códon representa. É redundante e quase universal entre os seres vivos. AUG (ATG no DNA) marca o início.'],
  ],
  [
    ['Transcrição', 'De DNA para RNA: cada T vira U (a RNA-polimerase lendo o gene). É o primeiro passo da expressão de um gene.'],
    ['Tradução', 'Leitura dos códons para montar a proteína, do códon de início (AUG) até um de parada (que o site marca com *).'],
    ['Fita complementar reversa', 'A fita que pareia com a sua (A-T, C-G), lida no sentido oposto — o molde usado pela célula.'],
    ['Conteúdo GC', 'Porcentagem de bases G e C. Pares G-C têm três pontes de hidrogênio (vs. duas de A-T), então regiões ricas em GC são mais estáveis.'],
    ['ORF', 'Janela de leitura aberta: de um ATG até um códon de parada, sem paradas no meio — uma candidata a gene.'],
    ['Frame de leitura', 'Como o códon tem 3 bases, a fita pode ser lida em três deslocamentos; cada um é um frame. O PaxLab varre os três.'],
  ],
  [
    ['FASTA', 'Formato de texto: uma linha com ">" e o nome, seguida das linhas com a sequência. É o que o NCBI devolve.'],
    ['Alinhamento', 'Encaixar duas sequências para revelar trechos em comum; lacunas (gaps) marcam inserções ou deleções.'],
    ['Identidade (%)', 'Proporção de colunas idênticas no alinhamento — quanto maior, mais parecidas as sequências.'],
    ['Needleman–Wunsch', 'Algoritmo de alinhamento global (sequências inteiras) que maximiza uma pontuação por programação dinâmica. A pontuação é configurável.'],
    ['Reprodutibilidade', 'Refazer uma análise e obter o mesmo resultado. Aqui cada análise guarda a receita e o permalink a re-executa para provar — simples porque as funções são puras.'],
  ],
];
Views.compendio = function () {
  const item = (t, d) => `<div class="py-2 border-b border-slate-100 last:border-0"><h4 class="font-semibold text-slate-800">${t}</h4><p class="text-sm text-slate-600">${d}</p></div>`;
  const group = (title, items) => `<section class="${card}"><h3 class="font-semibold mb-1">${title}</h3>${items.map(x => item(x[0], x[1])).join('')}</section>`;
  $app().innerHTML = `
    <section class="${card}"><h2 class="text-xl font-bold">📖 Compêndio</h2>
      <p class="text-slate-500">Os conceitos de biologia molecular e bioinformática necessários para entender o que o PaxLab mostra.</p></section>
    ${group('As moléculas e o código', COMPENDIO[0])}
    ${group('As operações do PaxLab', COMPENDIO[1])}
    ${group('Formatos, comparação e reprodutibilidade', COMPENDIO[2])}`;
};

// ---- Favoritos --------------------------------------------------------------
Views.favorites = async function () {
  _refresh = () => Views.favorites();
  const d = await api('GET', '/api/favorites');
  $app().innerHTML = `
    <section class="${card}"><h2 class="text-xl font-bold">⭐ Favoritos</h2>
      <p class="text-slate-500">Suas análises e alinhamentos favoritos.</p></section>
    <section class="${card}"><h3 class="font-semibold">Análises favoritas</h3>
      <div class="mt-2">${d.analyses.length ? d.analyses.map(analysisCard).join('') : '<p class="text-slate-500 text-sm">Nenhuma análise favoritada.</p>'}</div></section>
    <section class="${card}"><h3 class="font-semibold">Alinhamentos favoritos</h3>
      <ul class="mt-2 divide-y">${d.alignments.length ? d.alignments.map(r => `
        <li class="py-2 flex items-center gap-2">⭐
          <button onclick="viewAlign('${r.token}')" class="text-sky-600 font-medium">${esc(r.seqAName)} × ${esc(r.seqBName)}</button>
          <span class="text-xs text-slate-400">${r.identity.toFixed(1)}%</span></li>`).join('') : '<li class="py-2 text-slate-500 text-sm">Nenhum alinhamento favoritado.</li>'}</ul></section>`;
};

// ---- roteamento por hash (links de permalink: <origin>/#a/<token>) ----------
function handleHash() {
  const h = location.hash || '';
  let m;
  if ((m = h.match(/^#al\/([a-z0-9]+)/i))) { renderNav(); Views.alignView(m[1]); window.scrollTo(0, 0); return true; }
  if ((m = h.match(/^#a\/([a-z0-9]+)/i)))  { renderNav(); Views.permalink(m[1]); window.scrollTo(0, 0); return true; }
  return false;
}
window.addEventListener('hashchange', handleHash);

// ---- boot -------------------------------------------------------------------
if (!handleHash()) {
  if (State.token) go('home'); else Views.login();
}
