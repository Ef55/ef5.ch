function getEmail () {
  const b64_email = "ZGVzYW50b0BlZjUuY2g=";
  return window.atob(b64_email);
}

async function setMailto (item0) {
  const email = getEmail ();
  console.log(email);
  const item = item0.cloneNode(true);
  item.setAttribute('href', `mailto:${email}`);
  item.setAttribute('title', email);
  item.removeAttribute('onmouseover');
  item.removeAttribute('alt');
  // We replace the node to force an update of the link preview
  item0.replaceWith(item);
}
