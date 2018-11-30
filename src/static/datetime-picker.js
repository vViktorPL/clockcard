class DatetimePicker extends HTMLElement {
  static get observedAttributes() {
    return ['value'];
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (name === 'value') {
      this.input.setAttribute('value', dateToLocal(newValue));
    }
  }

  constructor() {
    super();

    const shadow = this.attachShadow({ mode: 'open' });

    const input = document.createElement('input');
    input.setAttribute('type', 'datetime-local');
    input.addEventListener('change', () => {
      const value = input.value !== '' ? JSON.parse(JSON.stringify(new Date(input.value))) : '';

      this.dispatchEvent(new CustomEvent('localizedChange', { detail: { value } }));
    });

    shadow.appendChild(input);
    this.input = input;

  }

  get value() {
    return this.input.value !== '' ? JSON.parse(JSON.stringify(new Date(this.input.value))) : '';
  }

  set value(newValue) {
    this.input.value = dateToLocal(newValue);
  }

  connectedCallback() {
    if (!this.hasAttribute('value')) {
      this.setAttribute('value', JSON.stringify(new Date()));
    }
  }

};

function dateToLocal(date) {
  if (!date) {
    return '';
  }

  const originalDate = new Date(date);
  originalDate.setMinutes(originalDate.getMinutes() - originalDate.getTimezoneOffset());

  const stringifiedDate = JSON.parse(JSON.stringify(originalDate));
  if (stringifiedDate === null) {
    return '';
  }

  const dateWithoutTZ = stringifiedDate.split('.')[0];
  return dateWithoutTZ.substring(0, dateWithoutTZ.lastIndexOf(':'));
}

customElements.define('datetime-picker', DatetimePicker);

module.exports.DateTimePicker = DatetimePicker;