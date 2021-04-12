import React, { useState, useEffect } from 'react';
import Modal from 'react-modal';

import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

function apiMemos() {
  return fetch('/api/memos', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

function apiMemoAdd(text_string) {
  return fetch('/api/memos/_add', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      'text_string': text_string
    })
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

///

function MemoList(props) {
  const { memos } = props;
  const rows = memos.map((a) => {
    return (
        <tr key={a.id}>
          <td>
            {a.id}
          </td>
          <td>
            <pre>
              {a.text_string}
            </pre>
          </td>
          <td>
            {a.created_on}
          </td>
        </tr>
    );
  });
  return (
      <table className="table table-striped">
        <thead>
          <tr>
            <th scope="col">Memo ID</th>
            <th scope="col">Text</th>
            <th scope="col">Created on</th>
          </tr>
        </thead>
        <tbody>{rows}</tbody>
      </table>
  );
}

function App () {
  const [textString, setTextString] = useState('');
  const [memosState, setMemosState] = useState([]);
  
  useEffect(() => {
    apiMemos().then((memos) =>  setMemosState(memos));
  }, []);

  function handleCreateMemoClick() {
    apiMemoAdd(textString).then(() => window.location.reload());
  }
    
  return (
      <div>
        <main className="p-md-5">
          <div className="container">

            <textarea value={textString}
                      onChange={(e) => setTextString(e.target.value)}>
            </textarea>
            <button type="button"
                    className="btn btn-primary"
                    onClick={() => handleCreateMemoClick()} >
              Create Memo
            </button>

            <MemoList memos={memosState} />
          </div>
        </main>
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));

