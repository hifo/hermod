let editors = [];
let editor_ids = {};

const CLIENT_ID = Math.floor(Math.random() * 1e9);

function get_xsrf() {
    const meta = document.querySelector(`meta[name='hoba:xsrf']`);
    if (meta) {
	return meta.content;
    }
    return null;
}

async function api_fetch(action, data) {
    data["action"] = action;
    data["xsrf"] = get_xsrf();
    data["client-id"] = CLIENT_ID;
    let success = false;
    let body = null;
    try {
	const response = await fetch("api.cgi", {
	    method: "POST",
	    body: JSON.stringify(data)});
	if (response.ok) {
	    body = await response.json();
	    if (body["success"]) {
		success = true;
	    } else {
		console.error("API Error message", body);
		alert("API Error message: " + body.msg);
	    }
	} else {
	    console.error("API Error, code ", response.status);
	}
    } catch (error) {
	console.error("API Error:", error);
    }
    return [success, body];
}

function create_post_editor(container, id) {
    container.classList.add("message");
    const editor = new Quill(container, { theme: "snow" });
    editor.db_id = id;
    editor.is_new = false;
    editor.unsaved = false;
    editor.on("text-change", function (delta, old_delta, source) {
	if (source != "restore") {
	    editor.unsaved = true;
	}
    });
    editors.push(editor);
    return editor;
}    

async function get_posts(client_id) {
    if (client_id && parseInt(client_id) == CLIENT_ID) {
	return;
    }
    const [success, res] = await api_fetch("get-posts", {});
    if (!success) {
	console.error("Error retrieving posts:", res);
    }
    for (const post of res.posts) {
	let editor;
	if (post.ID in editor_ids) {
	    editor = editor_ids[post.ID];
	} else {
	    editor = new_post();
	    editor.db_id = post.ID;
	    editor_ids[post.ID] = editor;
	}
	editor.is_new = false;
	editor.unsaved = false;
	editor.setContents(JSON.parse(post.DELTAS), "restore");
    }
}

function new_post(is_new) {
    const main = document.querySelector("main");
    const div = document.createElement("div");
    main.appendChild(div);
    const editor = create_post_editor(div);
    if (is_new) {
	editor.is_new = is_new;
    }
    return editor;
}

function click_new_post(e) {
    const editor = new_post(true);
    editor.container.scrollIntoView();
}

async function save_posts() {
    for (const editor of editors) {
	if (!editor.unsaved) {
	    continue;
	}
	if (editor.is_new) {
	    const [success, res] = await api_fetch("create-post", {
		"delta": JSON.stringify(editor.getContents()), // Second level of JSON string encoding used so it can go into the database directly.
	    });
	    if (success) {
		editor.is_new = false;
		editor.unsaved = false;
		editor.db_id = res["new-id"];
		new_post(true);
		editor_ids[editor.db_id] = editor;
	    }
	} else {
	    const [success, res] = await api_fetch("update-post", {
		"id": editor.db_id,
		"delta": JSON.stringify(editor.getContents()), // Second level of JSON string encoding used so it can go into the database directly.
	    });
	    if (success) {
		editor.unsaved = false;
	    } else {
		console.error("Unsaved", res);
	    }
	}
    }
}

function init_editors() {
    for (const container of document.querySelectorAll("div.message")) {
	create_post_editor(container);
    }
}

/* ******************
 * WEBSOCKETS CODE *
 */

let CONN = {
    ws: null,
    ready: false,
    prompt: null
};

function ws_message(e) {
    if (!CONN.ready) {
	if (e.data.trim() != "READY") {
	    throw new Error("Unexpected message from WebSocket connection: ", e.data);
	}
	CONN.ready = true;
	return;
    }
    const msg = e.data.split(" ");
    switch (msg[0].trim()) {
    case "UPDATE":
	get_posts(msg[1].trim());
	break;
    default:
	console.warn("Unknown message from WebSocket:", e.data.trim());
    }
}

function ws_close(e) {
    console.log("WebSocket closed.");
}

function connect() {
    const uri = new URL("/ws/hermod/hermod.lisp", location);
    if (location.protocol == "https:") {
	uri.protocol = "wss:";
    } else {
	uri.protocol = "ws:";
    }
    const ws = new WebSocket(uri);
    
    ws.onmessage = ws_message;
    ws.onclose = ws_close;

    CONN.ws = ws;
}

async function init() {
    init_editors();
    await get_posts();
    const editor = new_post(true);
    saving = setInterval(save_posts, 3000);
    connect();
}

init();
