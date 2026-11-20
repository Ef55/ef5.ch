function getEmail () {
  const b64_email = "ZGVzYW50b0BlZjUuY2g=";
  return window.atob(b64_email);
}

async function setMailto (item0) {
  const email = getEmail();
  const item = item0.cloneNode(true);
  item.setAttribute('href', `mailto:${email}`);
  item.setAttribute('title', email);
  item.removeAttribute('onmouseover');
  item.removeAttribute('onfocus');
  item.removeAttribute('tabindex'); // Focusable anyway, now that it has an href
  item.removeAttribute('alt');
  // We replace the node to force an update of the link preview; the copy
  // takes the focus over
  const focused = document.activeElement === item0;
  item0.replaceWith(item);
  if (focused) item.focus();
}

// CV: copy a publication's BibTeX (in data-bibtex), and say so in a small
// popup above the button (styled in css/default.css).
async function copyBibtex (button) {
  let status;
  try {
    await navigator.clipboard.writeText(button.dataset.bibtex);
    status = 'Copied!';
  } catch (e) {
    status = 'Could not copy';
  }
  button.querySelector('.copy-popup')?.remove(); // Remove older popups
  const popup = document.createElement('span');
  popup.className = 'copy-popup';
  popup.setAttribute('role', 'status');
  popup.textContent = status;
  button.append(popup);
  // Shown on the next frame, so that it fades in.
  requestAnimationFrame(() => popup.classList.add('shown'));
  setTimeout(() => {
    popup.classList.remove('shown');
    popup.addEventListener('transitionend', () => popup.remove(), { once: true });
  }, 1200);
}
