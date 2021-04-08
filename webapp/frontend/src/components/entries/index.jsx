import React, { useState, useEffect } from 'react';
import Modal from 'react-modal';

import ReactDOM from 'react-dom';
import 'bootstrap/dist/css/bootstrap.min.css';

function apiNotes() {
  return fetch('/api/notes', {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

function apiNoteCreate() {
  return fetch('/api/notes/_create', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    }
  }).then((resp) => resp.json()).then((body) => {
    return body.value;
  });
}

///

function NoteList(props) {
  const { notes } = props;
  const rows = notes.map((a) => {
    return (
        <tr key={a.id}>
          <td>
            {a.id}
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
            <th scope="col">Note ID</th>
            <th scope="col">Created on</th>
          </tr>
        </thead>
        <tbody>{rows}</tbody>
      </table>
  );
}

function App () {
  const [notesState, setNotesState] = useState([]);
  
  useEffect(() => {
    apiNotes().then((notes) =>  setNotesState(notes));
  }, []);

  function handleCreateNoteClick() {
    apiNoteCreate().then(() => window.location.reload());
  }
    
  return (
      <div>
        <main className="p-md-5">
          <div className="container">

            <button type="button"
                    className="btn btn-primary"
                    onClick={() => handleCreateNoteClick()} >
              Create Note
            </button>

            <NoteList notes={notesState} />
          </div>
        </main>
      </div>
  );
}

ReactDOM.render(
  <App />,
  document.getElementById('app'));

