import React, { useState, useEffect } from 'react';
import Modal from 'react-modal';

import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

import { Plus, Asterisk } from '../fa'

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

function apiMemoDetail(memoId) {
  return fetch('/api/memos/' + memoId, {
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

function MemoDetail(props) {
  const { memoId } = props;
  const [detail, setDetail] = useState(null);

    
  useEffect(() => {
    apiMemoDetail(memoId).then(setDetail);
  }, []);

  if (detail === null) {
    return (<div>Loading ...</div>);
  }

  return (
    <div>
      <ul>
        <li>ID: {memoId}</li>
        <li>Created on: {detail.created_on}</li>
      </ul>
      <pre>{detail.text.string}</pre>
    </div>
  );
}

function MemoList(props) {
  const { memos, onClickMemo } = props;
  const rows = []
  memos.forEach((ts_memos) => {
    const ts = ts_memos[0];
    rows.push((
      <tr key={ts}>
        <td>
        </td>
        <td>
          {ts}
        </td>
        <td>
        </td>
      </tr>
    ));
    ts_memos.slice(1).forEach((a) => {
        rows.push((
          <tr key={a.id}>
            <td>
              <pre>
                {a.text_string}
              </pre>
            </td>
            <td>
              {a.created_on}
            </td>
            <td>
              <button type="button"
                      className="btn btn-info"
                      aria-label="View detail"
                      onClick={() => onClickMemo(a.id)}>
                <Asterisk />
              </button>
            </td>
          </tr>
        ));
    });
  });
  return (
      <table className="table table-striped">
        <thead>
          <tr>
            <th scope="col">Text</th>
            <th scope="col">Created on</th>
            <th scope="col"></th>
          </tr>
        </thead>
        <tbody>{rows}</tbody>
      </table>
  );
}

function App () {
  const [textString, setTextString] = useState('');
  const [memosState, setMemosState] = useState([]);
  const [detailedMemoId, setDetailedMemoId] = useState(null);

  function refreshMemos() {
    apiMemos().then((memos) =>  setMemosState(memos));
  }

  function handleClickCreateMemo() {
    apiMemoAdd(textString).then(() => {
      setTextString('');
      refreshMemos();
    });
  }

  useEffect(() =>  refreshMemos(), []);

  return (
      <div>
        <main className="p-md-5">
          <div className="container">

            <textarea value={textString}
                      onChange={(e) => setTextString(e.target.value)}>
            </textarea>
            <div>
              <button type="button"
                      className="btn btn-primary"
                      aria-label="Add memo"
                      onClick={handleClickCreateMemo} >
                <Plus />
              </button>
            </div>

            <MemoList
              memos={memosState}
              onClickMemo={(memoId) => setDetailedMemoId(memoId)} />
          </div>
        </main>
        
        {
          detailedMemoId && (
            <Modal
              isOpen={true}
              onRequestClose={() => setDetailedMemoId(null)} >
              <MemoDetail memoId={detailedMemoId} />
            </Modal>
          )
        }
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));

